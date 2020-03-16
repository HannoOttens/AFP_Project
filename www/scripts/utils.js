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
 * @param {boolean} editable - If the grid should add a editable row
 *  */
function getList(url, target, idField, columns, editable) {
    $.ajax({
        method: "GET",
        url: url,
        success: (result) => {
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
                        html += '<td>' + val + '</td>';
                    }
                    else if (hd.template)
                        html += '<td>' + hd.template + '</td>';
                });
                html += '</tr>'
            });

            // Insert editable row if editable
            if (editable) {
                console.log(editable);
                // Insert headers
                html += '<tr><form id=newTargetForm></form>'; // TODO: Make generic
                columns.forEach((hd) => {
                    html += '<td>'
                    if (hd.editable) {
                        html += '<input form=newTargetForm name="' + hd.field + '" />'
                    }
                    html += '</td>';
                })
                html += '<td><button type=submit>Add target</button></td>'
                html += '</tr>'
            }

            // Set new html
            $(target).html(html);
        }
    });
}

/** Convert an interger to a data-string  */
function toDate(intDate) {
    var dateStr = (new Date(intDate)).toISOString()
    return dateStr.substring(11, 16) + ", " + dateStr.substring(0, 10);
}