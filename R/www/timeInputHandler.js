$(document).ready(function() {
    var inputSelector = '#start1';

    $(inputSelector).on('input', function(e) {
        var input = $(this).val().replace(/[^\d]/g, ''); // Remove non-digits
        var formatted = '';
        
        // Limit input length to 6 digits (HHMMSS)
        input = input.substring(0, 6);
        
        // Build the formatted string with colons after HH and MM
        for (var i = 0; i < input.length; i++) {
            if (i === 2 || i === 4) {
                formatted += ':'; // Add colon after HH and MM
            }
            formatted += input[i];
        }

        $(this).val(formatted);
        // Update placeholder to show the missing part
        $(this).attr('placeholder', placeholder.slice(formatted.length));
    });


    // Handle backspace key to move cursor correctly after colons
    $(inputSelector).on('keydown', function(e) {
        var val = $(this).val();
        if (e.keyCode === 8 && val.length && val[val.length - 1] === ':') { // if backspace and last char is colon
            e.preventDefault(); // prevent the backspace default action
            $(this).val(val.slice(0, -1)); // remove last char (the colon)
            this.selectionStart = this.selectionEnd = val.length - 1; // move cursor to the left
        }
    });
});
