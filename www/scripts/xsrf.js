// Set prefilter for XHR requests
$(() => {
    setXSRF();
});

function setXSRF() {
    var r = document.cookie.match(new RegExp('XSRF-TOKEN=([^;]+)'))
    if (r)
        $.ajaxPrefilter(function(opts, origOpts, xhr) {
            xhr.setRequestHeader('X-XSRF-TOKEN', r[1]);
        });
}
