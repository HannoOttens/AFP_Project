// https://developers.google.com/web/ilt/pwa/introduction-to-push-notifications
// https://developer.mozilla.org/en-US/docs/Web/API/Notifications_API/Using_the_Notifications_API
self.addEventListener('push', function(e) {
    var data = e.data.json();
  
    var options = {
      body: data.body,
      icon: (new URL(data.icon, data.url)).toString(),
      vibrate: [100, 50, 100],
      data: {
        url: (new URL(data.url)).toString(),
        dateOfArrival: Date.now(),
        primaryKey: 1
      },
      actions: [
        {action: 'open', title: 'Open website',
          icon: 'img/open.svg'},
        {action: 'close', title: 'Close',
          icon: 'img/xmark.svg'},
      ]
    };

    e.waitUntil(
      self.registration.showNotification(data.title, options)
    );
});

self.addEventListener('notificationclick', function(event) {
    if (!event.action) {
      return;
    }
    
    switch (event.action) {
      case 'open':
        const url = event.notification.data.url;
        const promiseChain = clients.openWindow(url);
        event.waitUntil(promiseChain);
        break;
      case 'close':
          event.notification.close();
        break;
      default:
        break;
    }
});
  