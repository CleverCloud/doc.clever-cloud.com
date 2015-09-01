## PostgreSQL plans

<table class="table table-bordered table-striped dataTable"><caption>PostgreSQL pricing plans</caption>
<tr>
<th>Name</th>
<th>Cache (Memory)</th>
<th>Disk</th>
<th>Conn. limit</th>
<th>Price /mo</th>
</tr>
<tr>
<td class="cc-col__price "><span class="label cc-label__price label-info">DEV</span></td>
<td>SHARED</td>
<td>256 MB</td>
<td>10</td>
<td>Free</td>
</tr>

<tr>
<td class="cc-col__price "><span class="label cc-label__price label-info">S</span></td>
<td>SHARED</td>
<td>1 GB</td>
<td>10</td>
<td>10 €</td>
</tr>
<tr>
<td class="cc-col__price "><span class="label cc-label__price label-info">M</span></td>
<td>1 GB</td>
<td>100 GB</td>
<td>75</td>
<td>30 €</td>
</tr>
<tr>
<td class="cc-col__price "><span class="label cc-label__price label-info">L</span></td>
<td>8 GB</td>
<td>450 GB</td>
<td>500</td>
<td>240 €</td>
</tr>
<tr>
<td class="cc-col__price "><span class="label cc-label__price label-info">XL</span></td>
<td>32 GB</td>
<td>600 GB</td>
<td>750</td>
<td>700 €</td>
</tr>
<tr>
<td class="cc-col__price "><span class="label cc-label__price label-info">XXL</span></td>
<td>64 GB</td>
<td>900 GB</td>
<td>1000</td>
<td>1200 €</td>
</tr>
</table>

### Migrating from an old database

Some applications require a populated database to run properly.
If you want to import your **SQL** dump, you can use several methods:

1. <a href="https://dbms-adminer.clever-cloud.com/adminer/">our WebGUI (Adminer)</a>
2. command line tool for PostgreSQL administration like `psql`
3. any PostgreSQL client such as pgAdmin 3.
