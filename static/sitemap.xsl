<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:html="http://www.w3.org/TR/REC-html40"
                xmlns:sitemap="http://www.sitemaps.org/schemas/sitemap/0.9"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" version="1.0" encoding="UTF-8" indent="yes" />

  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <title>Meadowstalk XML Sitemap</title>
        <meta http-equiv="Content-Type" content="text/html; encoding=utf-8" />
        <style>
          h1 {
            text-align: center;
          }

          body {
            font-family: Ubuntu, Verdana;
            font-size: 13px;
            text-align: center;
          }

          #content {
            width: 900px;
            margin-left: auto;
            margin-right: auto;
            text-align: justify;
          }

          #information {
            color: #111541;
            font-weight: bold;
            padding: 5px 10px 5px 10px;
            border: 1px solid #bfbfbf;
            border-radius: 5px;
          }

          td {
            font-size: 12px;
          }

          th {
            text-align: left;
            padding-right: 30px;
            font-size: 12px;
            background-color: #bfbfdf;
          }

          tr.alt {
            background-color: #cfcfff;
          }

          #url-table {
            margin-top: 10px;
            width: 900px;
            border: 1px solid #bfbfdf;
            border-radius: 5px;
          }

          tr:last-child > td:first-child {
            border-bottom-left-radius: 5px;
          }

          tr:last-child > td:last-child {
            border-bottom-right-radius: 5px;
          }

          tr:first-child > td:first-child {
            border-top-left-radius: 5px;
          }

          tr:first-child > td:last-child {
            border-top-right-radius: 5px;
          }
        </style>
      </head>
      <body>
        <div id="content">
          <h1>Meadowstalk XML Sitemap</h1>
          <div id="information">
            <p>
              This is the XML sitemap for <a href="https://meadowstalk.com">meadowstalk.com</a>. This file is designed to be processed by search engines.
            </p>
            <p>
              This sitemap is automatically generated by the Meadowstalk site.
            </p>
          </div>
          <table id="url-table" border="0" cellspacing="0" cellpadding="5">
            <tr>
              <th>URL</th>
              <th>Change Frequency</th>
              <th>Last Changed</th>
            </tr>

            <xsl:for-each select="/sitemap:urlset/sitemap:url">
              <tr>
                <xsl:if test="position() mod 2 != 1">
                  <xsl:attribute name="class">alt</xsl:attribute>
                </xsl:if>
                <td>
                  <a>
                    <xsl:attribute name="href">
                      <xsl:value-of select="sitemap:loc" />
                    </xsl:attribute>
                    <xsl:value-of select="sitemap:loc" />
                  </a>
                </td>
                <td>
                  <xsl:value-of select="sitemap:changefreq" />
                </td>
                <td>
                  <xsl:value-of select="concat(substring(sitemap:lastmod,0,11),concat(' ',substring(sitemap:lastmod,12,5)))" />
                </td>
              </tr>
            </xsl:for-each>
          </table>
        </div>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>


            
