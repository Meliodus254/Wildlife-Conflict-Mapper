Shiny.addCustomMessageHandler('geolocate', function(message) {
    if (navigator.geolocation) {
      navigator.geolocation.getCurrentPosition(
        function(position) {
          Shiny.setInputValue('geo', {
            latitude: position.coords.latitude,
            longitude: position.coords.longitude
          });
        },
        function(error) {
          alert('Error getting location: ' + error.message);
        }
      );
    } else {
      alert('Geolocation is not supported by this browser');
    }
  });