---
title: Clever Cloud Invoicing Update
position: 1
tags:
- support
---
# Unified Invoicing

In 2017, the billing system evolves. From multiple invoices created for each service and credit payment, Clever Cloud now create a **single invoice at the begining of the month**.

## How unified invoicing works

Each month, one invoiceis generated, including the whole activity (credits, add-on etc.) of the organisation.

## Monthly invoice:
At the begin of every month, a single new invoice is created. It provisions your account for the month to come:
* Credits provision based on the previous month consumption
* Optionally: regularisation for over consumption from the previous month
* Add-ons

### Example:

Add-ons with fixed monthly prices (eg. databases) will emit one line per add-on. Add-ons with pay as you go plans (cellar, FS buckets) will consume credits the same way apps do.

## One-shot invoices
When creating an add-on with a fixed monthly add-on price, an initial invoice is emitted for the rest of the month plus the next month. After this period, the add-on will be part of the monthly invoice.

### Example:
If you order a new database (€40 / month) on March 25, an invoice of €47.74 will be emitted, covering March 25 - April 30.
Starting May 1st, the database will be integrated in the monthly invoice.

## Payment method management:
You can register payment methods for each organisation. The monthly invoice is automatically paid with the default payment method. The other payment means can be used for one-shot invoices. This way, to use a new credit card, you just have to set it as the default one, it will automatically be used for new invoices.  

As of now, only credit cards can be used for monthly invoice payment. More will be added later.

## Over consumption / unpaid invoices:
If you desire a strong control on costs, you can set up a hard limit for consumption. If you reach it, applications will automatically be undeployed to prevent unexpected charges. By default this is disabled, you can activate it in the console.

## What will change with unified invoicing
Payment methods are now associated with organisations instead of users, which means you can see and change credit cards for organisations in an easier way.
Credit consumption and add-ons will be unified in a single monthly invoice. Each month, the consumption from the last month is credited on your account.  

**Going out of credits no longer stops applications (by default).**