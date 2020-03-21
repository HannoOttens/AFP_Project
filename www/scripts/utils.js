/**  Remove auth-cookie and return to login screen */
function logOut() {
    document.cookie = '';
    window.location.href = 'login.html';
}

/**  Get a list from the API
 * @param {string} url - The datasource URL
 * @param {string} target - The id of the DOM-target to inser the results into
 * @param {string} idField - Field that containts a unique identifier for the elements of the list (will be put in data-val of the tr)
 * @param {Array<object>} columns - A collection of name/field objects that specify the columns
 *      - name: Name of the column
 *      - field: Field to get the data from
 *      - templater: Function to construct HTML from the data field
 *      - tempalte: Static template for the column
 * @param {Function} editable - Function to send the grid data to
 *  */
function getList(url, target, idField, columns, editable) {
    $.ajax({
        method: "GET",
        url: url,
        success: (result) => {
            console.log(result);
            // Remove old
            $(target).empty();

            // Insert headers
            let html = '<tr>';
            columns.forEach((hd) => {
                html += '<th>' + (hd.name || "") + '</th>';
            })
            html += '</tr>'

            // Insert rows
            result.forEach((itm) => {
                html += '<tr data-val="' + itm[idField] + '">';
                columns.forEach((hd) => {
                    if (hd.field) {
                        let val = itm[hd.field];
                        if (hd.templater) val = hd.templater(val);
                        html += '<td data-value="' + val
                            + '" data-field="' + hd.field
                            + '" data-editable="' + !!hd.editable
                            + '">' + val + '</td>';
                    }
                    else if (hd.template)
                        html += '<td>' + hd.template + '</td>';
                });
                html += '</tr>'
            });

            // Insert editable row if editable
            if (editable) {
                // Insert headers
                html += '<tr><form id=editForm></form>'; // TODO: Make generic
                columns.forEach((hd) => {
                    html += '<td>'
                    if (hd.editable) {
                        html += '<input form=editForm name="' + hd.field + '" />'
                    }
                    html += '</td>';
                })
                html += '<td><button form=editForm type=submit>Add target</button></td>'
                html += '</tr>'
            }

            // Set new html
            $(target).html(html);

            $("#editForm").submit(function (event) {
                event.preventDefault();
                var formData = $(this).serialize();
                formData += "&" + idField + "=0"
                editable(formData);
            });
        }
    });
}

function inlineEdit(row, callback, idField) {
    let html = '<form id=inlineEditForm></form>'
    $(row).children().each((idx, item) => {
        let $item = $(item);
        let editable = $item.data("editable");
        let value = $item.data("value");
        let field = $item.data("field");

        if (editable)
            html += '<td><input form=inlineEditForm name="' + field + '" value="' + value + '" /></td>'
    });
    html += '<td><button form=inlineEditForm type=submit>Update</button></td>'

    // Replace with inline edit
    $(row).html(html);

    $("#inlineEditForm").submit(function (event) {
        event.preventDefault();
        var formData = $(this).serialize();
        formData += "&" + idField + "=" + $(row).data("val");
        callback(formData);
    });
}

/** Convert an interger to a data-string  */
function toDate(intDate) {
    var dateStr = (new Date(intDate)).toISOString()
    return dateStr.substring(11, 16) + ", " + dateStr.substring(0, 10);
}

