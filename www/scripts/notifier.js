// https://developers.google.com/web/ilt/pwa/introduction-to-push-notifications
// https://developer.mozilla.org/en-US/docs/Web/API/Notifications_API/Using_the_Notifications_API
self.addEventListener('push', function(e) {
    var body;
    if (e.data) {
      body = e.data.text();
    } else {
      body = 'Push message no payload';
    }
  
    var options = {
      body: body,
      icon: 'images/notification-flat.png',
      vibrate: [100, 50, 100],
      data: {
        dateOfArrival: Date.now(),
        primaryKey: 1
      },
      actions: [
        {action: 'explore', title: 'Explore this new world',
          icon: 'images/checkmark.png'},
        {action: 'close', title: 'I don\'t want any of this',
          icon: 'images/xmark.png'},
      ]
    };
    e.waitUntil(
      self.registration.showNotification('Push Notification', options)
    );
  });
  