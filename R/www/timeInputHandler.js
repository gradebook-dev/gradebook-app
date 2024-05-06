$(document).ready(function() {
    // This function applies the mask to the inputs
    var applyMask = function(input) {
        var value = input.val().replace(/[^\d]/g, ''); // Remove non-digits
        var formatted = '';
        
        // Limit input length to 6 digits (HHMMSS)
        value = value.substring(0, 6);
        
        // Build the formatted string with colons after HH and MM
        for (var i = 0; i < value.length; i++) {
            if (i === 2 || i === 4) {
                formatted += ':'; // Add colon after HH and MM
            }
            formatted += value[i];
        }
        
        input.val(formatted);
    };
    
    // Event delegation to handle dynamically added inputs on fields with IDs starting with "start" and "end""
     $(document).on('input', '[id^=start], [id^=end]', function() {
        applyMask($(this));
    });

    // Handle backspace key for all matching inputs
    $(document).on('keydown', '[id^=start]', function(e) {
        var val = $(this).val();
        if (e.keyCode === 8 && val.length && val[val.length - 1] === ':') {
            e.preventDefault();
            $(this).val(val.slice(0, -1));
            this.selectionStart = this.selectionEnd = val.length - 1;
        }
    });
});
