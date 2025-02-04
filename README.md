# CAR (Clean and Analyze in R) App
An integrated tool for automated Data Preprocessing, Analysis and Visualization using R Shiny

  <h2><strong>Overview</strong></h2>
    <p>CAR (Clean and Analyze in R) is an integrated tool designed for automated data preprocessing, analysis, and visualization, utilizing the powerful R Shiny framework. Developed as part of a master's thesis at the University of Macedonia, CAR aims to democratize data science by making advanced statistical methods and machine learning models accessible to users with minimal programming or statistical knowledge.</p>

  <h2><strong>Key Features</strong></h2>
    <ul>
        <li><strong>Automated Data Cleaning:</strong> Effortlessly handle missing values, data inconsistencies, and formatting issues.</li>
        <li><strong>Interactive Data Visualization:</strong> Generate customizable plots with interactive elements using Plotly and ggplot2.</li>
        <li><strong>Comprehensive Statistical Analysis:</strong> Perform descriptive statistics, hypothesis testing, ANOVA, and more.</li>
        <li><strong>Machine Learning Integration:</strong> Apply clustering, regression, SVM, CART, and Boost Gradient models.</li>
        <li><strong>User-Friendly Interface:</strong> Intuitive layout with drag-and-drop functionalities for seamless navigation.</li>
    </ul>

  <h2><strong>Installation</strong></h2>
    <ol>
        <li>Clone the repository:
            <pre><code>git clone https://github.com/your-repo/CAR-App.git</code></pre>
        </li>
        <li>Open the <code>app.R</code> file in RStudio.</li>
        <li>Install required packages:
            <pre><code>install.packages(c("shiny", "dplyr", "ggplot2", "plotly", "caret", "e1071", "rpart", "shinyWidgets", "DT", ...))</code></pre>
        </li>
        <li>Run the application:
            <pre><code>shiny::runApp('path_to_your_app_directory')</code></pre>
        </li>
    </ol>

  <h2><strong>How to Use</strong></h2>
    <ol>
        <li><strong>Load Data:</strong> Upload datasets in <code>.csv</code>, <code>.xls</code>, <code>.xlsx</code>, or <code>.sav</code> formats.</li>
        <li><strong>Data Cleaning:</strong> Choose between automated or manual cleaning options.</li>
        <li><strong>Descriptive Statistics:</strong> Analyze data characteristics with summary statistics.</li>
        <li><strong>Statistical Tests:</strong> Conduct t-tests, ANOVA, correlation analysis, and more.</li>
        <li><strong>Plotting:</strong> Create diverse plots including scatter plots, heatmaps, and time series graphs.</li>
        <li><strong>Machine Learning:</strong> Select algorithms, train models, and evaluate performance.</li>
    </ol>


  <h2><strong>Contributing</strong></h2>
    <p>CAR App is an ongoing project with many new features ready to be updated. Contributions are welcome! Please fork the repository, create a new branch for your feature, and submit a pull request.</p>

</body>
</html>
