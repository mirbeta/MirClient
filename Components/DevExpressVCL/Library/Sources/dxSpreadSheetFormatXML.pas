{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxSpreadSheetFormatXML;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Types, SysUtils, Classes, Graphics, dxCore, dxHashUtils, cxGraphics,
  dxSpreadSheetCore, dxSpreadSheetClasses, dxSpreadSheetUtils, dxSpreadSheetTextFileFormatCore, dxSpreadSheetFormatHTML,
  dxSpreadSheetCoreStyles, dxSpreadSheetStyles;

type

  { TdxSpreadSheetXMLFormat }

  TdxSpreadSheetXMLFormat = class(TdxSpreadSheetCustomFormat)
  public
    class function CanReadFromStream(AStream: TStream): Boolean; override;
    class function GetDescription: string; override;
    class function GetExt: string; override;
    class function GetReader: TdxSpreadSheetCustomReaderClass; override;
    class function GetWriter: TdxSpreadSheetCustomWriterClass; override;
  end;

  { TdxSpreadSheetXMLFormatWriter }

  TdxSpreadSheetXMLFormatWriter = class(TdxSpreadSheetHTMLFormatWriter)
  protected
    function CreateTableViewWriter(AView: TdxSpreadSheetTableView; const AArea: TRect): TdxSpreadSheetCustomFilerSubTask; override;
    function RegisterStyle(AStyle: TdxSpreadSheetCellStyle): string; override;
    procedure WriteDocumentFooter; override;
    procedure WriteDocumentHeader; override;
    procedure WriteStyleSheet(const AFileName: UnicodeString); virtual;
  end;

  { TdxSpreadSheetXMLFormatCellStyleWriter }

  TdxSpreadSheetXMLFormatCellStyleWriter = class(TdxSpreadSheetHTMLFormatCellStyleWriter)
  strict private
    FStyleName: string;
  protected
    procedure WriteBorders; override;
    procedure WriteBrush; override;
    procedure WriteFont; override;
    procedure WriteTextAlignment; override;
  public
    constructor Create(const AStyleName: string; AStreamWriter: TStreamWriter;
      AOwner: TdxSpreadSheetCustomFiler; AStyle: TdxSpreadSheetCellStyleHandle);
    procedure Execute; override;
  end;

  { TdxSpreadSheetXMLFormatTableViewWriter }

  TdxSpreadSheetXMLFormatTableViewWriter = class(TdxSpreadSheetHTMLFormatTableViewWriter)
  protected
    procedure WriteCell(ARowIndex: Integer; AColumnIndex: Integer; ACell: TdxSpreadSheetCell); override;
    procedure WriteContainer(AContainer: TdxSpreadSheetContainer; AContainerBounds: TRect); override;
    procedure WriteRowFooter(ARowIndex: Integer; ARow: TdxSpreadSheetTableRow); override;
    procedure WriteRowHeader(ARowIndex: Integer; ARow: TdxSpreadSheetTableRow); override;
    procedure WriteTableFooter; override;
    procedure WriteTableHeader; override;
  end;

implementation

uses
  Math, dxSpreadSheetGraphics;

type
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  TdxSpreadSheetTableColumnsAccess = class(TdxSpreadSheetTableColumns);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetTableViewInfoAccess = class(TdxSpreadSheetTableViewInfo);

const
  sdxXMLStyleTemplate =
    '<?xml version="1.0" encoding="UTF-8"?>' + #13#10 +
    '<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">' + #13#10 +
    '   <xsl:template match="/">' + #13#10 +
    '     <xsl:apply-templates select="CACHE" />' + #13#10 +
    '   </xsl:template>' + #13#10 +

    '   <xsl:template match="CACHE">' + #13#10 +
    '     <html>' + #13#10 +
    '        <head>' + #13#10 +
    '          <xsl:apply-templates select="TITLE" />' + #13#10 +
    '          <xsl:apply-templates select="STYLES" />' + #13#10 +
    '        </head>' + #13#10 +
    '        <body>' + #13#10 +
    '          <xsl:apply-templates select="LINES" />' + #13#10 +
    '        </body>' + #13#10 +
    '     </html>' + #13#10 +
    '   </xsl:template>' +  #13#10 +

    '   <xsl:template match="TITLE">' + #13#10 +
    '     <title>' + #13#10 +
    '       <xsl:value-of select="." />' + #13#10 +
    '     </title>' + #13#10 +
    '   </xsl:template>' + #13#10 +

    '   <xsl:template match="STYLES">' + #13#10 +
    '     <style type="text/css">' + #13#10 +
    '       table td { overflow: hidden; padding: 0px;}' + #13#10 +
    '       <xsl:apply-templates select="STYLE" />' + #13#10 +
    '     </style>' + #13#10 +
    '   </xsl:template>' + #13#10 +

    '   <xsl:template match="STYLE">' + #13#10 +
    '     .Style<xsl:value-of select="@Id" />' + #13#10 +
    '     { ' + #13#10 +
    '       border-style: solid;' + #13#10 +
    '       padding: <xsl:value-of select="@CellPadding" />;' + #13#10 +
    '       font-family: <xsl:value-of select="@FontName" />;' + #13#10 +
    '       mso-font-charset: <xsl:value-of select="@FontCharset" />;' + #13#10 +
    '       font-size: <xsl:value-of select="@FontSize" />pt;' + #13#10 +
    '       color: <xsl:value-of select="@FontColor" />;' + #13#10 +
    '       background-color: <xsl:value-of select="@BrushBkColor" />;' + #13#10 +
    '     <xsl:if test="@Bold=''True''">' + #13#10 +
    '       font-weight: bold;' + #13#10 +
    '     </xsl:if>' + #13#10 +
    '     <xsl:if test="@Italic=''True''">' + #13#10 +
    '       font-style: italic;' + #13#10 +
    '     </xsl:if>' + #13#10 +
    '     <xsl:if test="@Underline=''True''">' + #13#10 +
    '       text-decoration: underline;' + #13#10 +
    '     </xsl:if>' + #13#10 +
    '     <xsl:if test="@StrikeOut=''True''">' + #13#10 +
    '       text-decoration: line-through;' + #13#10 +
    '     </xsl:if>' + #13#10 +
    '     <xsl:apply-templates select="BORDER_LEFT" />' + #13#10 +
    '     <xsl:apply-templates select="BORDER_UP" />' + #13#10 +
    '     <xsl:apply-templates select="BORDER_RIGHT" />' + #13#10 +
    '     <xsl:apply-templates select="BORDER_DOWN" />' + #13#10 +
    '     }' + #13#10 +
    '   </xsl:template>' + #13#10 +

    '   <xsl:template match="BORDER_LEFT">' + #13#10 +
    '     border-left-width: <xsl:value-of select="@Width" />px;' + #13#10 +
    '     border-left-color: <xsl:value-of select="@Color" />;' + #13#10 +
    '   </xsl:template>' + #13#10 +

    '   <xsl:template match="BORDER_UP">' + #13#10 +
    '     border-top-width: <xsl:value-of select="@Width" />px;' + #13#10 +
    '     border-top-color: <xsl:value-of select="@Color" />;' + #13#10 +
    '   </xsl:template>' + #13#10 +

    '   <xsl:template match="BORDER_RIGHT">' + #13#10 +
    '     border-right-width: <xsl:value-of select="@Width" />px;' + #13#10 +
    '     border-right-color: <xsl:value-of select="@Color" />;' + #13#10 +
    '   </xsl:template>' + #13#10 +

    '   <xsl:template match="BORDER_DOWN">' + #13#10 +
    '     border-bottom-width: <xsl:value-of select="@Width" />px;' + #13#10 +
    '     border-bottom-color: <xsl:value-of select="@Color" />;' + #13#10 +
    '   </xsl:template>' + #13#10 +

    '   <xsl:template match="LINES">' + #13#10 +
    '     <table border="0" cellspacing="0" style="border-collapse: collapse;">' + #13#10 +
    '       <xsl:apply-templates select="LINE" />' + #13#10 +
    '     </table>' + #13#10 +
    '   </xsl:template>' + #13#10 +

    '   <xsl:template match="LINE">' + #13#10 +
    '     <tr>' + #13#10 +
    '       <xsl:attribute name="height"><xsl:value-of select="@Height" /></xsl:attribute>' + #13#10 +
    '       <xsl:apply-templates select="CELL" />' + #13#10 +
    '     </tr>' + #13#10 +
    '   </xsl:template>' + #13#10 +

    '   <xsl:template match="CELL">' + #13#10 +
    '     <td>' + #13#10 +
    '       <xsl:attribute name="nowrap"></xsl:attribute>' + #13#10 +
    '       <xsl:attribute name="width"><xsl:value-of select="@Width" /></xsl:attribute>' + #13#10 +
    '       <xsl:attribute name="height"><xsl:value-of select="@Height" /></xsl:attribute>' + #13#10 +
    '       <xsl:attribute name="align"><xsl:value-of select="@Align" /></xsl:attribute>' + #13#10 +
    '       <xsl:attribute name="colspan"><xsl:value-of select="@ColSpan" /></xsl:attribute>' + #13#10 +
    '       <xsl:attribute name="rowspan"><xsl:value-of select="@RowSpan" /></xsl:attribute>' + #13#10 +
    '       <xsl:attribute name="class">Style<xsl:value-of select="@StyleClass" /></xsl:attribute>' + #13#10 +
    '       <xsl:choose>' + #13#10 +
    '         <xsl:when test="LINES">' + #13#10 +
    '           <xsl:apply-templates select="LINES" />' + #13#10 +
    '         </xsl:when>' + #13#10 +
    '         <xsl:when test="IMAGE">' + #13#10 +
    '           <xsl:apply-templates select="IMAGE" />' + #13#10 +
    '         </xsl:when>' + #13#10 +
    '         <xsl:otherwise>' + #13#10 +
    '           <xsl:value-of select="." />' + #13#10 +
    '         </xsl:otherwise>' + #13#10 +
    '       </xsl:choose>' + #13#10 +
    '     </td>' + #13#10 +
    '   </xsl:template>' + #13#10 +

    '   <xsl:template match="IMAGE">' + #13#10 +
    '     <img>' + #13#10 +
    '       <xsl:attribute name="src"><xsl:value-of select="@Src" /></xsl:attribute>' + #13#10 +
    '       <xsl:value-of select="." />' + #13#10 +
    '     </img>' + #13#10 +
    '   </xsl:template>' + #13#10 +
    '</xsl:stylesheet>';

function GetXMLColor(AColor: TColor): string;
begin
  AColor := ColorToRGB(AColor);
  Result := Format('rgb(%d,%d,%d)', [GetRValue(AColor), GetGValue(AColor), GetBValue(AColor)]);
end;

function cxStrUnicodeNeeded(const AText: string; ACheckNormal: Boolean = False): Boolean;
const
  Normal = ['0'..'9', ':', ';', '*', '+', ',', '-', '.', '/', '!', ' ',  'A'..'Z', 'a'..'z', '_', '(', ')'];
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(AText) do
    if (Ord(AText[I]) > $7F) or (ACheckNormal and not dxCharInSet(AText[I], Normal)) then
    begin
      Result := True;
      Break;
    end
end;

function CheckedUnicodeString(const S: string): string; overload;
var
  I: Integer;
begin
  if cxStrUnicodeNeeded(S, True) then
  begin
    Result := '';
    for I := 1 to Length(S) do
      Result := Result + '&#' + IntToStr(Integer(S[I])) + ';';
  end
  else
    Result := S;
end;

function GetAlignText(AAlign: TdxSpreadSheetDataAlignHorz): string;
begin
  case AAlign of
    ssahCenter:
      Result := 'Center';
    ssahRight:
      Result := 'Right';
  else
    Result := 'Left';
  end;
end;

{ TdxSpreadSheetXMLFormat }

class function TdxSpreadSheetXMLFormat.CanReadFromStream(AStream: TStream): Boolean;
begin
  Result := False;
end;

class function TdxSpreadSheetXMLFormat.GetDescription: string;
begin
  Result := 'XML Document';
end;

class function TdxSpreadSheetXMLFormat.GetExt: string;
begin
  Result := '.xml';
end;

class function TdxSpreadSheetXMLFormat.GetReader: TdxSpreadSheetCustomReaderClass;
begin
  Result := nil;
end;

class function TdxSpreadSheetXMLFormat.GetWriter: TdxSpreadSheetCustomWriterClass;
begin
  Result := TdxSpreadSheetXMLFormatWriter;
end;

{ TdxSpreadSheetXMLFormatWriter }

function TdxSpreadSheetXMLFormatWriter.CreateTableViewWriter(
  AView: TdxSpreadSheetTableView; const AArea: TRect): TdxSpreadSheetCustomFilerSubTask;
begin
  Result := TdxSpreadSheetXMLFormatTableViewWriter.Create(Self, AView, AArea);
end;

function TdxSpreadSheetXMLFormatWriter.RegisterStyle(AStyle: TdxSpreadSheetCellStyle): string;
begin
  if not StyleMap.TryGetValue(AStyle.Handle, Result) then
  begin
    Result := IntToStr(StyleMap.Count);
    ExecuteSubTask(TdxSpreadSheetXMLFormatCellStyleWriter.Create(Result, StylesWriter, Self, AStyle.Handle));
    StyleMap.Add(AStyle.Handle, Result);
  end;
end;

procedure TdxSpreadSheetXMLFormatWriter.WriteDocumentFooter;
begin
  StreamWriter.WriteLine('</CACHE>');
end;

procedure TdxSpreadSheetXMLFormatWriter.WriteDocumentHeader;
var
  AStyleSheetFileName: string;
begin
  StreamWriter.WriteLine('<?xml version="1.0" encoding="UTF-8"?>');
  if Stream is TFileStream then
  begin
    AStyleSheetFileName := ChangeFileExt(TFileStream(Stream).FileName, '.xsl');
    WriteStyleSheet(AStyleSheetFileName);
    StreamWriter.WriteLine('<?xml-stylesheet type="text/xsl" href="%s"?>', [AStyleSheetFileName]);
  end;

  StreamWriter.WriteLine('<CACHE>');
  StreamWriter.WriteLine('<TITLE>%S</TITLE>', [SpreadSheet.ActiveSheetAsTable.Caption]);

  StreamWriter.WriteLine('<STYLES>');
  FStylesPositionInStream := StreamWriter.BaseStream.Position;
  StreamWriter.WriteLine('</STYLES>');
end;

procedure TdxSpreadSheetXMLFormatWriter.WriteStyleSheet(const AFileName: UnicodeString);
var
  AStream: TFileStream;
  AStreamWriter: TStreamWriter;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    AStreamWriter := TStreamWriter.Create(AStream, TEncoding.UTF8);
    try
      AStreamWriter.Write(sdxXMLStyleTemplate);
    finally
      AStreamWriter.Free;
    end;
  finally
    AStream.Free;
  end;
end;

{ TdxSpreadSheetXMLFormatCellStyleWriter }

constructor TdxSpreadSheetXMLFormatCellStyleWriter.Create(const AStyleName: string;
  AStreamWriter: TStreamWriter; AOwner: TdxSpreadSheetCustomFiler; AStyle: TdxSpreadSheetCellStyleHandle);
begin
  inherited Create(AOwner, AStreamWriter, AStyle);
  FStyleName := AStyleName;
end;

procedure TdxSpreadSheetXMLFormatCellStyleWriter.Execute;
begin
  StreamWriter.Write('<STYLE Id="%s"', [FStyleName]);
  WriteTextAlignment;
  WriteFont;
  WriteBrush;
  StreamWriter.WriteLine('>');
  WriteBorders;
  StreamWriter.WriteLine('</STYLE>');
end;

procedure TdxSpreadSheetXMLFormatCellStyleWriter.WriteBorders;
const
  SideName: array[TcxBorder] of string = (
    'BORDER_LEFT', 'BORDER_UP', 'BORDER_RIGHT', 'BORDER_DOWN'
  );
var
  ABorderColor: TColor;
  ADefaultBorderColor: TColor;
  ASide: TcxBorder;
begin
  if SpreadSheet.ActiveSheetAsTable.Options.ActualGridLines then
    ADefaultBorderColor := cxGetActualColor(SpreadSheet.OptionsView.GridLineColor, clBtnShadow)
  else
    ADefaultBorderColor := clNone;

  for ASide := Low(TcxBorder) to High(TcxBorder) do
  begin
    ABorderColor := cxGetActualColor(Style.Borders.BorderColor[ASide], ADefaultBorderColor);
    StreamWriter.WriteLine('<%s IsDefault="False" Color="%s" Width="%d"/>', [SideName[ASide], GetXMLColor(ABorderColor),
      IfThen(ABorderColor <> clNone, dxSpreadSheetBorderStyleThickness[Style.Borders.BorderStyle[ASide]])]);
  end;
end;

procedure TdxSpreadSheetXMLFormatCellStyleWriter.WriteBrush;
begin
  if Style.Brush.Style = sscfsSolid then
  begin
    StreamWriter.Write(' BrushStyle="Solid"');
    StreamWriter.Write(' BrushBkColor="' + GetXMLColor(cxGetActualColor(Style.Brush.BackgroundColor, ContentStyle.Color)) + '"');
    StreamWriter.Write(' BrushFgColor="' + GetXMLColor(cxGetActualColor(Style.Brush.ForegroundColor, ContentStyle.TextColor)) + '"');
  end;
end;

procedure TdxSpreadSheetXMLFormatCellStyleWriter.WriteFont;
const
  FontStyleNameMap: array[TFontStyle] of string = (
    'Bold', 'Italic', 'Underline', 'StrikeOut'
  );
var
  AStyle: TFontStyle;
  ATextColor: TColor;
begin
  ATextColor := Style.Font.Color;
  if not cxColorIsValid(ATextColor) then
    ATextColor := ContentStyle.TextColor;
  StreamWriter.Write(' FontName="%s"', [CheckedUnicodeString(Style.Font.Name)]);
  StreamWriter.Write(' FontCharset="%d"', [Style.Font.Charset]);
  for AStyle := Low(TFontStyle) to High(TFontStyle) do
    StreamWriter.Write(' %s="%s"', [FontStyleNameMap[AStyle], BoolToStr(AStyle in Style.Font.Style, True)]);
  StreamWriter.Write(' FontColor="%s"', [GetXMLColor(ATextColor)]);
  StreamWriter.Write(' FontSize="%d"', [Style.Font.Size]);
end;

procedure TdxSpreadSheetXMLFormatCellStyleWriter.WriteTextAlignment;
begin
  StreamWriter.Write(' AlignText="%s"', [GetAlignText(Style.AlignHorz)]);
end;

{ TdxSpreadSheetXMLFormatTableViewWriter }

procedure TdxSpreadSheetXMLFormatTableViewWriter.WriteCell(ARowIndex, AColumnIndex: Integer; ACell: TdxSpreadSheetCell);

  function GetInscribedContainer: TdxSpreadSheetContainer;
  begin
    if not InscribedContainers.TryGetValue(ACell, Result) then
      Result := nil;
  end;

var
  AAlignHorz: TdxSpreadSheetDataAlignHorz;
  ACellDisplayText: string;
  AContainer: TdxSpreadSheetContainerAccess;
  AContainerBounds: TRect;
  AFileName: string;
  AMergedCell: TdxSpreadSheetMergedCell;
  ARootPath: string;
begin
  AMergedCell := View.MergedCells.FindCell(ARowIndex, AColumnIndex);
  if (AMergedCell <> nil) and ((AMergedCell.Area.Left <> AColumnIndex) or (AMergedCell.Area.Top <> ARowIndex)) then
    Exit;

  StreamWriter.Write('<CELL StyleClass="%s" Width="%d"', [GetCellStyleName(ARowIndex, AColumnIndex, ACell),
    TdxSpreadSheetTableColumnsAccess(View.Columns).GetItemSize(AColumnIndex)]);

  if ACell <> nil then
  begin
    AAlignHorz := ACell.Style.AlignHorz;
    if (AAlignHorz = ssahGeneral) and ACell.IsNumericValue then
      AAlignHorz := ssahRight;
    StreamWriter.Write(' Align="%s"', [GetAlignText(AAlignHorz)]);
  end;

  if AMergedCell <> nil then
  begin
    if dxSpreadSheetAreaWidth(AMergedCell.Area) > 1 then
      StreamWriter.Write(' ColSpan="%d"', [dxSpreadSheetAreaWidth(AMergedCell.Area)]);
    if dxSpreadSheetAreaHeight(AMergedCell.Area) > 1 then
      StreamWriter.Write(' RowSpan="%d"', [dxSpreadSheetAreaHeight(AMergedCell.Area)]);
  end;
  StreamWriter.Write('>');

  if ACell <> nil then
    ACellDisplayText := CheckedUnicodeString(ACell.DisplayText)
  else
    ACellDisplayText := '';

  AContainer := TdxSpreadSheetContainerAccess(GetInscribedContainer);
  if (AContainer <> nil) and Owner.GetImageFileName(ARootPath, AFileName) then
  begin
    AContainerBounds := AContainer.Calculator.CalculateBounds;
    PrepareContainerImage(ARootPath + AFileName, AContainerBounds, AContainer);
    StreamWriter.WriteLine('<IMAGE Src="%s">%s</IMAGE>', [AFileName, ACellDisplayText]);
  end
  else
    StreamWriter.Write(ACellDisplayText);

  StreamWriter.WriteLine('</CELL>');
end;

procedure TdxSpreadSheetXMLFormatTableViewWriter.WriteContainer(AContainer: TdxSpreadSheetContainer; AContainerBounds: TRect);
begin
  // do nothing, only inscribed containers are supported
end;

procedure TdxSpreadSheetXMLFormatTableViewWriter.WriteRowFooter(ARowIndex: Integer; ARow: TdxSpreadSheetTableRow);
begin
  StreamWriter.WriteLine('</LINE>');
end;

procedure TdxSpreadSheetXMLFormatTableViewWriter.WriteRowHeader(ARowIndex: Integer; ARow: TdxSpreadSheetTableRow);
begin
  StreamWriter.WriteLine('<LINE Height="%d">', [FCurrentRowSize]);
end;

procedure TdxSpreadSheetXMLFormatTableViewWriter.WriteTableFooter;
begin
  StreamWriter.WriteLine('</LINES>');
end;

procedure TdxSpreadSheetXMLFormatTableViewWriter.WriteTableHeader;
begin
  StreamWriter.WriteLine('<LINES ColCount="%d" RowCount="%d">', [Area.Right + 1, Area.Bottom + 1]);
end;

initialization
  TdxSpreadSheetXMLFormat.Register;

finalization
  TdxSpreadSheetXMLFormat.Unregister;
end.
