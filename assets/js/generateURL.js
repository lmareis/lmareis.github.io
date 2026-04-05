document.addEventListener('DOMContentLoaded', function() {
    // Define the generateUrl function
    function generateUrl() {
        const checkboxes = document.querySelectorAll('.city-list input[type="checkbox"]');
        let selectedTerms = [];

        // Iterate over the checkboxes and collect the values of the selected ones
        checkboxes.forEach(checkbox => {
            if (checkbox.checked) {
                // Prepend '%2B' (encoded plus) before each selected term
                selectedTerms.push(`%2B%22${encodeURIComponent(checkbox.value)}%22`);
            }
        });

        if (selectedTerms.length > 0) {
            // Join the selected terms to form the search query
            const query = selectedTerms.join(' '); // Separate terms with space

            // Create the full Zenodo search URL
            const url = `https://zenodo.org/communities/mardigmci/records?q=${query}&l=list&p=1&s=10&sort=bestmatch`;

            // Open the URL in a new tab
            window.open(url, '_blank');
        } else {
            alert('Please select at least one attribute.');
        }
    }

    // Attach the event listener to the button after the DOM is fully loaded
    const button = document.querySelector('.generate-btn');
    if (button) {
        button.addEventListener('click', generateUrl);
    }
});
