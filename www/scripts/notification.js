// https://developers.google.com/web/fundamentals/push-notifications/subscribing-a-user

function subscribe() {
    if (!('serviceWorker' in navigator)) {
        // Service Worker isn't supported on this browser, disable or hide UI.
        Console.log("Service workers are not supported")
        return;
    }
    
    if (!('PushManager' in window)) {
        // Push isn't supported on this browser, disable or hide UI.
        Console.log("Push is not supported")
        return;
    }
    
    askPermission()
    .then(getPublicKey)
    .then(subscribeUser)
    .then(postSubscription)
    .then(() => alert("Successfully subscribed!"))
    .catch(err => alert(err));
}

// Get public key of server to register service worker
function getPublicKey() {
    return new Promise(function(resolve, reject) {
        setXSRF();
        $.ajax("/notification/keys", {
            method: 'GET',
            success: function (data) {
                applicationServerKey = new Uint8Array(data);
                resolve(applicationServerKey); 
            },
            error: reject 
        });
    });
}

// Pass details to send notifications to server
function postSubscription(sub) {
    return new Promise(function(resolve, reject) {
        setXSRF();
        $.ajax("/notification/subscribe", {
            method: 'POST',
            data: sub,
            success: function (success) {
                resolve(success)
            },
            error: reject
        });
    });
}

// Ask for permission to send notifications
function askPermission() {
    return new Promise(function(resolve, reject) {
        const permissionResult = Notification.requestPermission(function(result) {
        resolve(result);
    });

    if (permissionResult) {
        permissionResult.then(resolve, reject);
    }
    })
    .then(function(permissionResult) {
        if (permissionResult !== 'granted') {
            throw new Error('We weren\'t granted permission.');
        }
    });
}

// Register service worker and subscribe user
function subscribeUser(pubKey) {
    navigator.serviceWorker.register('/notifier.js');

    return navigator.serviceWorker.ready.then(
        function (registration) {
            const subscribeOptions = {
                userVisibleOnly: true,
                applicationServerKey: pubKey
            };
            return registration.pushManager.subscribe(subscribeOptions);
    })
    .then(function(pushSubscription) {
        details = {  
            endpoint: pushSubscription.endpoint, 
            hash: pushSubscription.toJSON().keys.p256dh, 
            auth: pushSubscription.toJSON().keys.auth
        };
        return details;
    });
}