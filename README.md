# Food Delivery Analytics Dashboard – Modern UI v2 – Modern Food-App UI

A complete R/Shiny food-delivery analytics project with a custom modern FOODLY-style frontend.

## Features
- Modern top navigation and responsive layout
- Food-themed orange/red visual design
- Date, city, cuisine and order-status filters
- KPI cards for Orders, Revenue, Average Order Value and Rating
- Monthly performance, cuisine mix, city performance and order status charts
- Revenue vs profit, payment method and delivery-time/rating analysis
- Top restaurant ranking and restaurant scorecard
- Filtered orders table with CSV download

## Requirements
- R 4.6.1 or compatible R version
- Internet access once to install packages

## Windows / VS Code
1. Open this folder in VS Code.
2. Open Terminal.
3. Run:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe" requirements.R
```

4. Start the dashboard:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe" run_app.R
```

5. Open `http://127.0.0.1:8080` in your browser.

## Project structure
- `app.R` – main Shiny application
- `run_app.R` – launcher
- `requirements.R` – package installer using a user library
- `data/food_delivery.csv` – sample dataset
- `www/styles.css` – custom frontend styling
- `www/nav.js` – navigation behavior
