

/* 
 * Create sortable HTML table using Dynatable from JSON dataset. 
 *
 * Includes functionality for setting custom display formatting
 * of cells and of contents. 
 * This is accessed by setting the 'data-cellwriter' or 'data-contentformat'
 * attributes in the HTML column header elements to the name of the 
 * desired writer function. 
 * 
 * One type of custom display is a graphical representation of the data 
 * embedded in a table cell (eg. single bar plot). 
 * 
 * Data is supplied to populate the table by setting the 'data-content' 
 * attribute on the table element with the path to the JSON file 
 * as its value. The object in the JSON file should either be the data itself
 * (an array of objects representing rows) or contain the key "data" whose
 * value is an array of objects.
 */
(function() {
  
  /* Formatters to use in displaying data values. 
  * Accessed by setting data-contentformat attribute in header to key. 
  */
  var cellContentFormatters = {
    // Format larger numbers by separating groups of 10^3 with commas.
    countFormat: function(num) {
      return num.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
    },
    
    // Draw a coloured bar extending the specified width percentage 
    // in the parent container. 
    // Also specify class to apply. 
    bar: function(width, barClass) {
      return "<div class='" + barClass + "' style='width: " + width + "%'></div>";
    }
  };
  
  /* Functions to generate custom table cells. 
  * Accessed by setting data-cellwriter attribute in header to key. 
  */
  var cellWriterFunctions = {
    // Represents a proportion as a single-bar miniplot. 
    propBar: function(column, record) {
      var td = "<td class='bar_cell'";
      if(column.hidden) {
        td += " style='display: none;'";
      }
      td += "><div class='bar_container'>" + 
        cellContentFormatters.bar(record[column.id] * 100, "bar barcol0") +
        "<div style='clear:both'></div></div></td>";
      
      return td;
    },
    
    // Represents an array of multiple proportions in a stacked-bar miniplot.
    // Supports a maximum of 8 separate colours. 
    multiBar: function(column, record) {
      var td = "<td class='bar_cell'";
      if(column.hidden) {
        td += " style='display: none;'";
      }
      td += "><div class='bar_container'>";
      
      var vals = record[column.id];
      if(!Array.isArray(vals)) {
        vals = [ vals ];
      }
      // Limit number of values to 8. 
      if(vals.length > 8) {
        vals = vals.splice(0, 8);
      }
      
      vals = vals.map(function(x) { return x * 100; });
      td = vals.reduce(function(prevStr, currVal, i) {
          return prevStr + 
            cellContentFormatters.bar(currVal, "bar barcol" + i);
        }, td) +
        "<div style='clear:both'></div></div></td>";
      
      return td;
    }
  };
  
  /* Map of column names to elements of cellWriterFunctions - 
   * to be populated from HTML attributes. */
  var columnCellWriters = {};
  
  /* Custom cell writer to delegate cell generation according to 
   * column-specific writers. 
   */
  function myCellWriter(column, record, defaultCellWriter) {
    if(columnCellWriters.hasOwnProperty(column.id)) {
      return cellWriterFunctions[columnCellWriters[column.id]](column, record);
    } else {
      return defaultCellWriter(column, record);
    }
  };
  
  /* Custom row writer to allow passing cell writing to custom writers, 
   * defaulting to the default Dynatable cell writer. 
   */
  function myRowWriter(rowIndex, record, columns, cellWriter) {
    return "<tr>" + 
      columns.reduce(function(prevStr, currCol) {
        return prevStr + myCellWriter(currCol, record, cellWriter);
      }, "") +
      "</tr>";
  };
      
  /* Generate tables. */
  $(function() {
    // Select any table objects that are present. 
    var $table = $("table");
    
    if(!$table.length) {
      // No tables in document. 
      return;
    }
    
    // Set table defaults. 
    $.dynatableSetup({
      dataset: {
        // Reset pagination defaults. 
        perPageDefault: 100,
        perPageOptions: [100,500,1000]
      },
      table: {
        // Don't necessarily use the same alignments as for the headers. 
        copyHeaderAlignment: false,
        // Apply header classes to all rows.
        copyHeaderClass: true,
        // Look for column headings in last of possibly multiple header rows.
        headRowSelector: "thead tr:last-child"
      }
    });
    
    // Render each table. 
    $table.each(function() {
      var $currentTable = $(this),
        jsonFile = $currentTable.data("content");
      
      if(!jsonFile) {
        // Could not find data associated with table. 
        // Render empty table.
        $currentTable.dynatable();
      }
      
      // Load data from JSON. 
      $.getJSON(jsonFile)
        .done(function(json) {
          json = json.data || json;
          
          var dtParams = {
            dataset: {
              // If contains 'data' key, use the value as the data. 
              // Otherwise assume the entire JSON represents the data. 
              records: json
            }
          };
          
          // Decide on pagination based on size of dataset. 
          if(json.length <= 100) {
            // Small dataset - do not paginate.
            dtParams.features = { paginate: false };
          } else {
            dtParams.dataset.perPageOptions = [50, 100, 500, 1000];
            dtParams.dataset.perPageDefault = 100;
          }
          
          // Read formatting options from data attributed supplied to column headers. 
          
          // Object 'writers' stores individual column formatters 
          // keyed by column ID, and possibly the custom row writer. 
          // It gets passed as a Dynatable option. 
          var writers = {};
          
          // If there are multiple header rows, look up the last one. 
          var $headers = $currentTable.find("thead > tr:last-child > th");
          var elt, fmt, colId;
          
          // Look up content formatters.
          var $formatHeaders = $headers.filter("[data-contentformat]");
          
          if($formatHeaders.length) {
            // If any, look up the referenced formatter function, 
            // and map it against the column ID.
            $formatHeaders.each(function() {
              elt = $(this);
              fmt = elt.data("contentformat");
              
              // If the specified formatter is not currently defined, ignore it.
              if(cellContentFormatters.hasOwnProperty(fmt)) {
                colId = elt.attr("data-dynatable-column");
                // Generate a writer function for this column using the 
                // specified formatter. 
                writers[colId] = (function() {
                  var formatter = cellContentFormatters[fmt];
                  return function(record) {
                      return formatter(record[colId]);
                  };
                })();
              }
            });
          }
            
          // Look up cell writers. 
          // If any, store these for later reference. 
          $formatHeaders = $headers.filter("[data-cellwriter]");
          
          if($formatHeaders.length) {
            $formatHeaders.each(function() {
              elt = $(this);
              fmt = elt.data("cellwriter");
              // If the specified formatter is not currently defined, ignore it.
              if(cellWriterFunctions.hasOwnProperty(fmt)) {
                // Map the column ID to the name of the cell writer function. 
                columnCellWriters[elt.attr("data-dynatable-column")] = fmt;
              } 
            });
          }
          
          // Set rowWriter according to whether or not we have custom cell writers. 
          if(!$.isEmptyObject(columnCellWriters)) {
            writers["_rowWriter"] = myRowWriter;
          } 
          
          // Add writers to table if any. 
          if(!$.isEmptyObject(writers)) {
            dtParams.writers = writers;
          }
          
          // Generate the table. 
          $currentTable.dynatable(dtParams);
        })
        .fail(function() {
          // Failed to load JSON. 
          // Render empty table.
          $currentTable.dynatable();
        });
    });
  });
})();

