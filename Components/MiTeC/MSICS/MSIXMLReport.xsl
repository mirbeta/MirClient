<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/">
    <html>
    <head>
    <meta name="generator" content="MiTeC System Information Component Suite"/>
    <meta name="author" content="MiTeC"/>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
    </head>
    <body>
    <center>
      <h2><xsl:value-of select="SystemInfoReport/title"/></h2>

      <table width="100%" style="border: thin solid #999999" cellpadding="5" cellspacing="0">
        <xsl:for-each select="SystemInfoReport/category">
        <tr>
          <xsl:apply-templates select="category"/>
          <td style="background-color: #FFEFDF; border: 0 solid #999999; border-bottom: thin; font-size: larger"><xsl:value-of select="@title"/></td>
        </tr>
        <xsl:for-each select="section">
        <tr>
          <td>
            <table cellpadding="2" cellspacing="0" width="100%">
              <tr>
                <td style="border: 0 dotted #999999; border-bottom-width: thin; font-size: larger; font-weight: bold"><xsl:value-of select="@title"/></td>
              </tr>
              <xsl:for-each select="recordset">
              <tr>
                <td>
                  <table cellpadding="2" cellspacing="0" width="100%">
                    <tr>
                      <td style="font-weight: bold"><xsl:value-of select="@title"/></td>
                    </tr>
                    <tr>
                      <td>
                        <table border="0" cellspacing="2" cellpadding="2" width="100%"  style="border: thin solid #CCCCCC" bgcolor="#EEEEEE">
                          <tr bgcolor="#6699cc">
                            <xsl:for-each select="fieldname">
                              <th align="left"><font size="1" face="arial" color="white">
                              <xsl:apply-templates/>
                              </font></th>
                            </xsl:for-each>
                          </tr>
                          <xsl:for-each select="datarow">
                          <tr>
						    <xsl:if test="@hl='yes'">
                              <xsl:attribute name="bgcolor">#e5e5e5</xsl:attribute>
                            </xsl:if>
                            <xsl:for-each select="fieldvalue">
                            <td><font size="2" face="verdana"><xsl:apply-templates/></font></td>
                            </xsl:for-each>
                          </tr>
                          </xsl:for-each>
                        </table>
                      </td>
                    </tr>
                  </table>
                </td>
              </tr>
              </xsl:for-each>
            </table>
          </td>
        </tr>
        </xsl:for-each>
        </xsl:for-each>
      </table>
    </center>
    </body>
    </html>
</xsl:template>
</xsl:stylesheet>
