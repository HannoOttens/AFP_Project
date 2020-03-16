$(() => {
    // Set XSRF for every request
    setXSRF();
});

/** Set up AJAX to use XSRF token (and to log out after a 401/403) */
function setXSRF() {
    // Redirect back to the login after a UNAUTHORIZED error
    $.ajaxSetup({
        error: function (x, status, error) {
            if (x.status == 401 || x.status == 403) {
                alert("Your session has expired, you will be returned to the account screen.");
                window.location.href ="login.html";
            }
        }
    });
    // Add the current XRSF token to the request for verification
    $.ajaxPrefilter(function (opts, origOpts, xhr) {
        var r = document.cookie.match(new RegExp('XSRF-TOKEN=([^;]+)'));
        if (r)
            xhr.setRequestHeader('X-XSRF-TOKEN', r[1]);
    });
}
