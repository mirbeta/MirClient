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

unit dxSpreadSheetFormatXLSXWriterComments;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxGDIPlusClasses, dxCoreGraphics, dxXMLDoc, dxSpreadSheetContainers, dxSpreadSheetFormatXLSXWriter,
  dxSpreadSheetFormatXLSXTags, dxSpreadSheetPackedFileFormatCore, dxSpreadSheetClasses, dxSpreadSheetCoreStyles,
  dxXMLWriter;

type

  { TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder }

  TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder = class(TdxSpreadSheetXLSXWriterCustomXMLBuilder)
  strict private
    FAuthors: TStringList;
    FComments: TList<TdxSpreadSheetCommentContainer>;
  protected
    class function GetContentRelationship: AnsiString; override;
    class function GetContentType: AnsiString; override;

    procedure PopulateAuthors;
    procedure WriteAuthors(AWriter: TdxXmlWriter); virtual;
    procedure WriteComment(AWriter: TdxXmlWriter; AComment: TdxSpreadSheetCommentContainer); virtual;
    procedure WriteCommentText(AWriter: TdxXmlWriter; AComment: TdxSpreadSheetCommentTextBox); virtual;
    procedure WriteComments(AWriter: TdxXmlWriter); virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder;
      AOwnerRels: TdxSpreadSheetXLSXWriterRels; const ATargetFileName: AnsiString);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure ExecuteCore(AWriter: TdxXmlWriter); override;
    //
    property Authors: TStringList read FAuthors;
    property Comments: TList<TdxSpreadSheetCommentContainer> read FComments;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder }

  TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder = class(TdxSpreadSheetXLSXWriterCustomBuilder)
  strict private
    FContainer: TdxSpreadSheetCommentContainer;
    FNode: TdxXMLNode;
  protected
    function EncodeAnchors: string;
    function EncodeColor(const AColor: TdxAlphaColor): string;
    function EncodeOpacity(AAlpha: Byte): string;
    procedure WriteClientData(ANode: TdxXMLNode); virtual;
    procedure WriteShapeFill(AFill: TdxGPBrush); virtual;
    procedure WriteShapeStroke(AStroke: TdxGPPen); virtual;
    procedure WriteStyleAttributes; virtual;
    procedure WriteTextBox(ANode: TdxXMLNode); virtual;
  public
    constructor Create(AContainer: TdxSpreadSheetCommentContainer; ANode: TdxXMLNode;
      AOwner: TdxSpreadSheetXLSXWriter; AOwnerRels: TdxSpreadSheetXLSXWriterRels);
    procedure Execute; override;
    //
    property Container: TdxSpreadSheetCommentContainer read FContainer;
    property Node: TdxXMLNode read FNode;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetTableViewLegacyDrawingBuilder }

  TdxSpreadSheetXLSXWriterWorksheetTableViewLegacyDrawingBuilder = class(TdxSpreadSheetXLSXWriterWorksheetTableViewSubFileBuilder)
  strict private
    function GetComments: TList<TdxSpreadSheetCommentContainer>;
  protected
    procedure WriteDefaults(ANode: TdxXMLNode); virtual;
    procedure WriteShapes(ANode: TdxXMLNode; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
  public
    procedure Execute; override;
    //
    property Comments: TList<TdxSpreadSheetCommentContainer> read GetComments;
  end;

implementation

uses
  dxSpreadSheetUtils, dxSpreadSheetFormatUtils, AnsiStrings, dxSpreadSheetCore, cxGeometry, dxColorPicker,
  dxSpreadSheetFormatXLSXReaderDrawing, dxCore, cxGraphics, dxStringHelper;

type
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);

{ TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder }

constructor TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder.Create(
  AOwner: TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder; AOwnerRels: TdxSpreadSheetXLSXWriterRels;
  const ATargetFileName: AnsiString);
begin
  inherited Create(AOwner.Owner, AOwnerRels, ATargetFileName);
  FComments := AOwner.Comments;
end;

destructor TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder.Destroy;
begin
  FreeAndNil(FAuthors);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder.AfterConstruction;
begin
  inherited AfterConstruction;
  FAuthors := TStringList.Create;
end;

class function TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder.GetContentRelationship: AnsiString;
begin
  Result := sdxXLSXCommentsRelationship;
end;

class function TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder.GetContentType: AnsiString;
begin
  Result := sdxXLSXCommentsContentType;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder.ExecuteCore(AWriter: TdxXmlWriter);
begin
  PopulateAuthors;

  AWriter.WriteStartElement(sdxXLSXNodeComments, sdxXLSXWorkbookNamespace);
  try
    WriteAuthors(AWriter);
    WriteComments(AWriter);
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder.PopulateAuthors;
var
  AAuthor: string;
  I: Integer;
begin
  for I := 0 to Comments.Count - 1 do
  begin
    AAuthor := Comments[I].Author;
    if Authors.IndexOf(AAuthor) < 0 then
      Authors.Add(AAuthor);
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder.WriteAuthors(AWriter: TdxXmlWriter);
var
  I: Integer;
begin
  AWriter.WriteStartElement(sdxXLSXNodeAuthors);
  try
    for I := 0 to Authors.Count - 1 do
      AWriter.WriteElementString(sdxXLSXNodeAuthor, Authors[I]);
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder.WriteComment(
  AWriter: TdxXmlWriter; AComment: TdxSpreadSheetCommentContainer);
begin
  AWriter.WriteStartElement(sdxXLSXNodeComment);
  try
    AWriter.WriteAttributeInteger(sdxXLSXAttrShapeId, 0);
    AWriter.WriteAttributeInteger(sdxXLSXAttrAuthorId, Authors.IndexOf(AComment.Author));
    AWriter.WriteAttributeString(sdxXLSXAttrRef, AComment.Cell.GetReference(False));
    WriteCommentText(AWriter, AComment.TextBox);
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder.WriteCommentText(
  AWriter: TdxXmlWriter; AComment: TdxSpreadSheetCommentTextBox);
var
  AString: TdxSpreadSheetFormattedSharedString;
begin
  AWriter.WriteStartElement(sdxXLSXNodeTextFull);
  try
    AString := TdxSpreadSheetFormattedSharedString.CreateObject(AComment.TextAsString);
    try
      AString.Runs.Add.FontHandle := AComment.Font.Handle;
      ExecuteSubTask(TdxSpreadSheetXLSXWriterFormattedStringBuilder.Create(Owner, OwnerRels, AWriter, AString));
    finally
      AString.Free;
    end;
  finally
    AWriter.WriteEndElement;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentsBuilder.WriteComments(AWriter: TdxXmlWriter);
var
  I: Integer;
begin
  AWriter.WriteStartElement(sdxXLSXNodeCommentList);
  try
    for I := 0 to Comments.Count - 1 do
      WriteComment(AWriter, Comments[I]);
  finally
    AWriter.WriteEndElement;
  end;
end;

{ TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder }

constructor TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder.Create(
  AContainer: TdxSpreadSheetCommentContainer; ANode: TdxXMLNode;
  AOwner: TdxSpreadSheetXLSXWriter; AOwnerRels: TdxSpreadSheetXLSXWriterRels);
begin
  inherited Create(AOwner, AOwnerRels);
  FContainer := AContainer;
  FNode := ANode;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder.Execute;
var
  ASubNode: TdxXMLNode;
begin
  Node.Attributes.SetValueAsString('type', '#_x0000_t202');

  WriteStyleAttributes;
  WriteShapeFill(Container.Shape.Brush);
  WriteShapeStroke(Container.Shape.Pen);

  ASubNode := Node.AddChild('v:path');
  ASubNode.Attributes.SetValue('o:connecttype', 'none');

  WriteTextBox(Node.AddChild(sdxXLSXNodeVMLTextBox));
  WriteClientData(Node.AddChild(sdxXLSXNodeXClientData));
end;

function TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder.EncodeAnchors: string;

  procedure CalculateAnchor(AAnchor: TdxSpreadSheetContainerAnchorPoint; const P: TPoint; var AAnchorPoint, AAnchorOffset: TPoint);
  var
    ATableView: IdxSpreadSheetTableView;
  begin
    AAnchorOffset := AAnchor.Offset;
    if Supports(Container.Parent, IdxSpreadSheetTableView, ATableView) then
    begin
      if ATableView.GetCellAtAbsolutePoint(P, AAnchorPoint.Y, AAnchorPoint.X) then
        AAnchorOffset := cxPointOffset(P, ATableView.GetAbsoluteCellBounds(AAnchorPoint.Y, AAnchorPoint.X, False).TopLeft, False);
    end;
  end;

var
  AAnchorOffsets: TRect;
  AAnchorPoints: TRect;
  AContainerBounds: TRect;
begin
  AContainerBounds := TdxSpreadSheetContainerAccess(Container).Calculator.CalculateBounds;
  CalculateAnchor(Container.AnchorPoint1, AContainerBounds.TopLeft, AAnchorPoints.TopLeft, AAnchorOffsets.TopLeft);
  CalculateAnchor(Container.AnchorPoint2, AContainerBounds.BottomRight, AAnchorPoints.BottomRight, AAnchorOffsets.BottomRight);

  Result := Format('%d, %d, %d, %d, %d, %d, %d, %d', [
    AAnchorPoints.Left, AAnchorOffsets.Left, AAnchorPoints.Top, AAnchorOffsets.Top,
    AAnchorPoints.Right, AAnchorOffsets.Right, AAnchorPoints.Bottom, AAnchorOffsets.Bottom]);
end;

function TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder.EncodeColor(const AColor: TdxAlphaColor): string;
begin
  Result := '#' + TdxColorHelper.AlphaColorToHexCode(AColor, False, False);
end;

function TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder.EncodeOpacity(AAlpha: Byte): string;
begin
  Result := IntToStr(MulDiv(AAlpha, MaxWord, MaxByte)) + 'f';
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder.WriteClientData(ANode: TdxXMLNode);
begin
  if Container.AnchorType in [catOneCell, catAbsolute] then
    ANode.AddChild(sdxXLSXNodeXSizeWithCells);
  if Container.AnchorType in [catAbsolute] then
    ANode.AddChild(sdxXLSXNodeXMoveWithCells);

  ANode.Attributes.SetValueAsString('ObjectType', 'Note');
  ANode.AddChild(sdxXLSXNodeXAnchor).TextAsString := EncodeAnchors;
  ANode.AddChild(sdxXLSXNodeXAutoFill).Text := 'False';
  ANode.AddChild(sdxXLSXNodeXTextHAlign).TextAsString := dxXLSXVMLTextAlignHorzMap[Container.TextBox.AlignHorz];
  ANode.AddChild(sdxXLSXNodeXTextVAlign).TextAsString := dxXLSXVMLTextAlignVertMap[Container.TextBox.AlignVert];
  ANode.AddChild(sdxXLSXNodeXRow).TextAsString := IntToStr(Container.Cell.RowIndex);
  ANode.AddChild(sdxXLSXNodeXColumn).TextAsString := IntToStr(Container.Cell.ColumnIndex);
  if Container.Visible then
    ANode.AddChild(sdxXLSXNodeXVisible);
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder.WriteShapeFill(AFill: TdxGPBrush);
const
  AnglesMap: array[TdxGPBrushGradientMode] of Integer = (270, 180, 225, 135);
  TypeMap: array[TdxGPBrushStyle] of string = (
    sdxXLSXValueSolid, sdxXLSXValueGradient, sdxXLSXValueTile, sdxXLSXValueSolid
  );
var
  AFillNode: TdxXMLNode;
begin
  AFillNode := Node.AddChild(sdxXLSXNodeVMLFill);
  AFillNode.Attributes.SetValueAsString(sdxXLSXAttrTypeLC, TypeMap[AFill.Style]);

  case AFill.Style of
    gpbsTexture:
      AFillNode.Attributes.SetValue(sdxXLSXAttrORelID, WriteImage(AFill.Texture));

    gpbsSolid:
      begin
        Node.Attributes.SetValueAsString(sdxXLSXAttrFillColor, EncodeColor(AFill.Color));
        if dxGetAlpha(AFill.Color) <> MaxByte then
          AFillNode.Attributes.SetValueAsString(sdxXLSXAttrOpacity, EncodeOpacity(dxGetAlpha(AFill.Color)));
      end;

    gpbsGradient:
      if AFill.GradientPoints.Count > 1 then
      begin
        Node.Attributes.SetValueAsString(sdxXLSXAttrFillColor,
          EncodeColor(AFill.GradientPoints.Colors[0]));
        AFillNode.Attributes.SetValueAsString(sdxXLSXAttrOpacity,
          EncodeOpacity(dxGetAlpha(AFill.GradientPoints.Colors[0])));
        AFillNode.Attributes.SetValueAsString(sdxXLSXAttrColor2,
          EncodeColor(AFill.GradientPoints.Colors[AFill.GradientPoints.Count - 1]));
        AFillNode.Attributes.SetValueAsString(sdxXLSXAttrOOpacity2,
          EncodeOpacity(dxGetAlpha(AFill.GradientPoints.Colors[AFill.GradientPoints.Count - 1])));
        AFillNode.Attributes.SetValueAsInteger(sdxXLSXAttrAngle, AnglesMap[AFill.GradientMode]);
     end;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder.WriteShapeStroke(AStroke: TdxGPPen);
begin
  Node.Attributes.SetValueAsString(sdxXLSXAttrStrokeColor, EncodeColor(AStroke.Brush.Color));
  Node.Attributes.SetValueAsString(sdxXLSXAttrStrokeWeight,
    IntToStr(Round(TdxValueUnitsHelper.PixelsToPoints(AStroke.Width))) + 'pt');
  Node.AddChild(sdxXLSXNodeVMLStroke).Attributes.SetValueAsString(
    sdxXLSXAttrDashStyle, dxXLSXVMLDashStyle[AStroke.Style]);
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder.WriteStyleAttributes;
const
  VisibilityMap: array[Boolean] of string = ('hidden', 'visible');
var
  ABuilder: TStringBuilder;
  AContainerBounds: TRect;
begin
  AContainerBounds := TdxSpreadSheetContainerAccess(Container).Calculator.CalculateBounds;
  ABuilder := TdxStringBuilderManager.Get;
  try
    ABuilder.Append('position:absolute;');
    ABuilder.Append(Format('margin-left:%0.2fpt;', [TdxValueUnitsHelper.PixelsToPoints(AContainerBounds.Left)], dxInvariantFormatSettings));
    ABuilder.Append(Format('margin-top:%0.2fpt;', [TdxValueUnitsHelper.PixelsToPoints(AContainerBounds.Top)], dxInvariantFormatSettings));
    ABuilder.Append(Format('width:%0.2fpt;', [TdxValueUnitsHelper.PixelsToPoints(cxRectWidth(AContainerBounds))], dxInvariantFormatSettings));
    ABuilder.Append(Format('height:%0.2fpt;', [TdxValueUnitsHelper.PixelsToPoints(cxRectHeight(AContainerBounds))], dxInvariantFormatSettings));
    ABuilder.AppendFormat('z-index:%d;', [Container.Index + 1]);
    ABuilder.AppendFormat('visibility:%s;', [VisibilityMap[Container.Visible]]);

    Node.Attributes.SetValueAsString(sdxXLSXAttrStyle, ABuilder.ToString);
  finally
    TdxStringBuilderManager.Release(ABuilder);
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder.WriteTextBox(ANode: TdxXMLNode);
begin
  if not cxRectIsEqual(dxEMUToPixels(dxXLSXDefaultTextPadding), Container.TextBox.ContentOffsets) then
  begin
    ANode.Attributes.SetValueAsString(sdxXLSXAttrInset, Format('%dmm,%dmm,%dmm,%dmm', [
      TdxValueUnitsHelper.PixelsToMillimeters(Container.TextBox.ContentOffsets.Left),
      TdxValueUnitsHelper.PixelsToMillimeters(Container.TextBox.ContentOffsets.Top),
      TdxValueUnitsHelper.PixelsToMillimeters(Container.TextBox.ContentOffsets.Right),
      TdxValueUnitsHelper.PixelsToMillimeters(Container.TextBox.ContentOffsets.Bottom)]));
  end;

  if Container.TextBox.AutoSize then
    ANode.Attributes.SetValue(sdxXLSXAttrStyle, sdxXLSXMSOFitShapeToText + ':t');

  ANode.AddChild('div').Attributes.SetValueAsString(sdxXLSXAttrStyle,
    'text-align:' + LowerCase(dxXLSXVMLTextAlignHorzMap[Container.TextBox.AlignHorz]));
end;

{ TdxSpreadSheetXLSXWriterWorksheetTableViewLegacyDrawingBuilder }

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewLegacyDrawingBuilder.Execute;
var
  ADoc: TdxXMLDocument;
  ANode: TdxXMLNode;
  ARels: TdxSpreadSheetXLSXWriterRels;
begin
  ADoc := TdxSpreadSheetXMLDocument.Create(nil);
  ARels := TdxSpreadSheetXLSXWriterRels.Create(nil);
  try
    ANode := ADoc.Root.AddChild('xml');
    ANode.Attributes.SetValue('xmlns:v', 'urn:schemas-microsoft-com:vml');
    ANode.Attributes.SetValue('xmlns:o', 'urn:schemas-microsoft-com:office:office');
    ANode.Attributes.SetValue('xmlns:x', 'urn:schemas-microsoft-com:office:excel');

    WriteDefaults(ANode);
    WriteShapes(ANode, ARels);

    RegisterFile(TargetFileName, '', sdxXLSXVMLDrawingRelationship, OwnerRels);
    WriteXML(TargetFileNameRels, ARels);
    WriteXML(TargetFileName, ADoc);
  finally
    ARels.Free;
    ADoc.Free;
  end;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewLegacyDrawingBuilder.WriteDefaults(ANode: TdxXMLNode);
var
  ASubNode: TdxXMLNode;
begin
  ASubNode := ANode.AddChild('o:shapelayout');
  ASubNode.Attributes.SetValueAsString('v:ext', 'edit');
  ASubNode := ASubNode.AddChild('o:idmap');
  ASubNode.Attributes.SetValueAsInteger('data', 1);
  ASubNode.Attributes.SetValueAsString('v:ext', 'edit');

  ASubNode := ANode.AddChild('v:shapetype');
  ASubNode.Attributes.SetValueAsString('id', '_x0000_t202');
  ASubNode.Attributes.SetValueAsString('coordsize', '21600,21600');
  ASubNode.Attributes.SetValueAsString('o:spt', '202');
  ASubNode.Attributes.SetValueAsString('path', 'm,l,21600r21600,l21600,xe');

  ASubNode := ASubNode.AddChild('v:stroke');
  ASubNode.Attributes.SetValueAsString('joinstyle', 'miter');
  ASubNode := ASubNode.Parent;

  ASubNode := ASubNode.AddChild('v:path');
  ASubNode.Attributes.SetValueAsString('gradientshapeok', 't');
  ASubNode.Attributes.SetValueAsString('o:connecttype', 'rect');
end;

procedure TdxSpreadSheetXLSXWriterWorksheetTableViewLegacyDrawingBuilder.WriteShapes(
  ANode: TdxXMLNode; ARels: TdxSpreadSheetXLSXWriterRels);
var
  I: Integer;
begin
  for I := 0 to Comments.Count - 1 do
    ExecuteSubTask(TdxSpreadSheetXLSXWriterWorksheetTableViewCommentContainerBuilder.Create(
      Comments[I], ANode.AddChild(sdxXLSXNodeVMLShape), Owner, ARels));
end;

function TdxSpreadSheetXLSXWriterWorksheetTableViewLegacyDrawingBuilder.GetComments: TList<TdxSpreadSheetCommentContainer>;
begin
  Result := OwnerWriter.Comments;
end;

end.
