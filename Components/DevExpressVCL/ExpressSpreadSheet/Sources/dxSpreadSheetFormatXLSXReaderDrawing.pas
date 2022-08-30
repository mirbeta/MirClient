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

unit dxSpreadSheetFormatXLSXReaderDrawing;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Graphics, dxCore, dxCoreClasses, cxClasses, dxCustomTree, dxXMLDoc, dxZIPUtils,
  dxSpreadSheetCore, dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetStrs, dxSpreadSheetPackedFileFormatCore,
  dxSpreadSheetUtils, dxGDIPlusClasses, Generics.Defaults, Generics.Collections, dxCoreGraphics, cxGeometry, dxHashUtils, Variants,
  dxSpreadSheetPrinting, dxSpreadSheetConditionalFormattingRules, dxSpreadSheetContainers, dxSpreadSheetFormatXLSXReader,
  dxSpreadSheetHyperlinks, dxSpreadSheetCoreStyles;

const
  dxXLSXDefaultTextPadding: TRect = (Left: 91440; Top: 45720; Right: 91440; Bottom: 45720);

type

  { TdxSpreadSheetXLSXReaderDrawingParser }

  TdxSpreadSheetXLSXReaderDrawingParser = class(TdxSpreadSheetXLSXReaderCustomDocumentParser)
  strict private
    FView: TdxSpreadSheetTableView;
  protected
    procedure ProcessContainer(ANode: TdxXMLNode; AUserData: Pointer); virtual;
  public
    constructor Create(const AFileName: AnsiString; AOwner: TdxSpreadSheetXLSXReader; AView: TdxSpreadSheetTableView); reintroduce;
    procedure Execute; override;
    //
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetXLSXReaderContainerParser }

  TdxSpreadSheetXLSXReaderContainerParser = class(TdxSpreadSheetXLSXReaderCustomDocumentSubParser)
  strict private
    function GetOwnerParser: TdxSpreadSheetXLSXReaderDrawingParser; inline;
    procedure ReadAnchorMarker(ANode: TdxXMLNode; AAnchor: TdxSpreadSheetContainerAnchorPoint);
  protected
    function CreateContainer: TdxSpreadSheetContainer; virtual; abstract;
    procedure ReadContainerRestrictions(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer); virtual;
    procedure ReadContent(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer); virtual; abstract;
    procedure ReadDescription(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer); virtual;
    procedure ReadEditAsMode(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer); virtual;
    procedure ReadHyperlink(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer); virtual;
    procedure ReadPlacement(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer); virtual;
    function ReadPoint(ANode: TdxXMLNode; const XName, YName: TdxXMLString): TPoint;
    //
    procedure ReadAbsoluteAnchor(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
    procedure ReadOneCellAnchor(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
    procedure ReadTwoCellAnchor(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
  public
    procedure Execute; override;
    //
    property OwnerParser: TdxSpreadSheetXLSXReaderDrawingParser read GetOwnerParser;
  end;

  { TdxSpreadSheetXLSXReaderDrawingShapeContainerParser }

  TdxSpreadSheetXLSXReaderDrawingShapeContainerParser = class(TdxSpreadSheetXLSXReaderContainerParser)
  protected
    function CreateContainer: TdxSpreadSheetContainer; override;
    procedure ReadContent(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer); override;
    procedure ReadShapeDefaultStyles(ANode: TdxXMLNode; AShape: TdxSpreadSheetShape); virtual;
    procedure ReadShapeFillParameters(ANode: TdxXMLNode; AShape: TdxSpreadSheetShape); virtual;
    procedure ReadShapeGeometry(ANode: TdxXMLNode; AShape: TdxSpreadSheetShape); virtual;
    procedure ReadShapeLineParameters(ANode: TdxXMLNode; APen: TdxGPPen); virtual;
    procedure ReadShapeProperties(ANode: TdxXMLNode; AContainer: TdxSpreadSheetShapeContainer); virtual;
    procedure ReadTransformProperties(ANode: TdxXMLNode; AContainer: TdxSpreadSheetShapeContainer); virtual;
  end;

  { TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser }

  TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser = class(TdxSpreadSheetXLSXReaderDrawingShapeContainerParser)
  protected
    function CreateContainer: TdxSpreadSheetContainer; override;
    procedure ReadContent(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer); override;
    procedure ReadTextBox(ANode: TdxXMLNode; ATextBox: TdxSpreadSheetTextBox); virtual;
  end;

  { TdxSpreadSheetXLSXReaderDrawingPictureContainerParser }

  TdxSpreadSheetXLSXReaderDrawingPictureContainerParser = class(TdxSpreadSheetXLSXReaderDrawingShapeContainerParser)
  protected
    function CreateContainer: TdxSpreadSheetContainer; override;
    procedure ReadContent(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer); override;
    procedure ReadPicture(ANode: TdxXMLNode; APicture: TdxSpreadSheetPicture); virtual;
    procedure ReadPictureContainerDescription(ANode: TdxXMLNode; AContainer: TdxSpreadSheetPictureContainer); virtual;
    procedure ReadPictureContainerProperties(ANode: TdxXMLNode; AContainer: TdxSpreadSheetPictureContainer); virtual;
    procedure ReadPictureResource(ANode: TdxXMLNode; APicture: TdxSpreadSheetPicture); virtual;
  end;

  { TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextParser }

  TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextParser = class(TdxSpreadSheetXLSXReaderRichTextParser)
  strict private
    FOwner: TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser;
  protected
    function CreateFontParser(AFont: TdxSpreadSheetFontHandle; ANode: TdxXMLNode): TdxSpreadSheetXLSXReaderFontParser; override;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser);
    class function Parse(AOwner: TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser;
      ANode: TdxXMLNode): TdxSpreadSheetFormattedSharedString;
    //
    property Owner: TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser read FOwner;
  end;

  { TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextFontParser }

  TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextFontParser = class(TdxSpreadSheetXLSXReaderFontParser)
  strict private
    FOwner: TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextParser;

    procedure ReadFontColor(ANode: TdxXMLNode);
  public
    constructor Create(AFont: TdxSpreadSheetFontHandle; ANode: TdxXMLNode;
      AOwner: TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextParser);
    procedure Execute; override;
    //
    property Owner: TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextParser read FOwner;
  end;

implementation

uses
  AnsiStrings, Math, dxColorPicker, cxGraphics, dxSpreadSheetGraphics, dxSpreadSheetFormulas,
  dxSpreadSheetFormatXLSX, dxSpreadSheetFormatXLSXTags, dxSpreadSheetFormatUtils, dxSpreadSheetCoreStrs;

type
  TdxSpreadSheetCustomReaderAccess = class(TdxSpreadSheetCustomReader);
  TdxSpreadSheetPictureAccess = class(TdxSpreadSheetPicture);

{ TdxSpreadSheetXLSXReaderDrawingParser }

constructor TdxSpreadSheetXLSXReaderDrawingParser.Create(
  const AFileName: AnsiString; AOwner: TdxSpreadSheetXLSXReader; AView: TdxSpreadSheetTableView);
begin
  inherited Create(AFileName, AOwner);
  FView := AView;
end;

procedure TdxSpreadSheetXLSXReaderDrawingParser.Execute;
begin
  if Node <> nil then
    Node.ForEach(ProcessContainer);
end;

procedure TdxSpreadSheetXLSXReaderDrawingParser.ProcessContainer(ANode: TdxXMLNode; AUserData: Pointer);
var
  ASubNode: TdxXMLNode;
begin
  if ANode.FindChild(sdxXLSXNodeDrawingPictureContainer) <> nil then
    ExecuteSubTask(TdxSpreadSheetXLSXReaderDrawingPictureContainerParser.Create(ANode, Self))
  else

  if ANode.FindChild(sdxXLSXNodeDrawingShapeContainer, ASubNode) or
     ANode.FindChild([sdxXLSXNodeDrawingShapeGroup, sdxXLSXNodeDrawingShapeContainer], ASubNode) then
  begin
    if ASubNode.FindChild(sdxXLSXNodeDrawingTextBody) <> nil then
      ExecuteSubTask(TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser.Create(ANode, Self))
    else
      ExecuteSubTask(TdxSpreadSheetXLSXReaderDrawingShapeContainerParser.Create(ANode, Self));
  end;
end;

{ TdxSpreadSheetXLSXReaderContainerParser }

procedure TdxSpreadSheetXLSXReaderContainerParser.Execute;
var
  AContainer: TdxSpreadSheetContainer;
  AContentNode: TdxXMLNode;
begin
  AContainer := CreateContainer;
  AContainer.BeginUpdate;
  try
    ReadPlacement(Node, AContainer);
    AContentNode := Node.FindChild(sdxXLSXNodeDrawingShapeGroup);
    if AContentNode = nil then
      AContentNode := Node;
    ReadContent(AContentNode, AContainer);
  finally
    AContainer.EndUpdate;
  end;
end;

procedure TdxSpreadSheetXLSXReaderContainerParser.ReadContainerRestrictions(
  ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
var
  AIndex: TdxSpreadSheetContainerRestriction;
begin
  for AIndex := Low(TdxSpreadSheetContainerRestriction) to High(TdxSpreadSheetContainerRestriction) do
  begin
    if ANode.Attributes.GetValueAsBoolean(dxXLSXRestrictionNames[AIndex]) then
      AContainer.Restrictions := AContainer.Restrictions + [AIndex]
    else
      AContainer.Restrictions := AContainer.Restrictions - [AIndex];
  end;
end;

procedure TdxSpreadSheetXLSXReaderContainerParser.ReadDescription(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
var
  AChildNode: TdxXMLNode;
begin
  AContainer.Name := ANode.Attributes.GetValueAsString(sdxXLSXAttrName);
  AContainer.Description := ANode.Attributes.GetValueAsString(sdxXLSXAttrAlternateText);
  AContainer.Title := ANode.Attributes.GetValueAsString(sdxXLSXAttrTitle);
  if ANode.FindChild(sdxXLSXNodeDrawingShapeHLink, AChildNode) then
    ReadHyperlink(AChildNode, AContainer);
  AContainer.Visible := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrHidden, False);
end;

procedure TdxSpreadSheetXLSXReaderContainerParser.ReadEditAsMode(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
var
  AEditAs: TdxXMLString;
begin
  AEditAs := ANode.Attributes.GetValue(sdxXLSXAttrEditAs, sdxXLSXValueEditAsTwoCell);
  AContainer.AnchorPoint1.FixedToCell := SameText(AEditAs, sdxXLSXValueEditAsOneCell) or SameText(AEditAs, sdxXLSXValueEditAsTwoCell);
  AContainer.AnchorPoint2.FixedToCell := SameText(AEditAs, sdxXLSXValueEditAsTwoCell);
end;

procedure TdxSpreadSheetXLSXReaderContainerParser.ReadHyperlink(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
var
  AValue: string;
  AAttr: TdxXMLNodeAttribute;
  AHyperlink: TdxSpreadSheetHyperlink;
  ARelsItem: TdxSpreadSheetXLSXReaderRelsItem;
begin
  AHyperlink := OwnerParser.View.Hyperlinks.Add(cxInvalidRect);
  if ANode.Attributes.Find(sdxXLSXAttrTooltip, AAttr) then
    AHyperlink.ScreenTip := AAttr.ValueAsString;
  AValue := '';
  if ANode.Attributes.Find(sdxXLSXAttrRId, AAttr) then
  begin
    if OwnerParser.Rels.Find(AAttr.Value, ARelsItem) then
      AValue := dxXMLStringToString(ARelsItem.FileName);
    if Pos('#', AValue) = 1 then
      AValue[1] := '=';
  end;
  AContainer.Hyperlink := AHyperlink;
  Owner.Hyperlinks.AddObject(AValue, AHyperlink);
end;

procedure TdxSpreadSheetXLSXReaderContainerParser.ReadPlacement(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
begin
  AContainer.BeginUpdate;
  try
    if SameText(ANode.Name, sdxXLSXNodeAnchorTwoCell) then
      ReadTwoCellAnchor(ANode, AContainer)
    else

    if SameText(ANode.Name, sdxXLSXNodeAnchorOneCell) then
      ReadOneCellAnchor(ANode, AContainer)
    else

    if SameText(ANode.Name, sdxXLSXNodeAnchorAbsolute) then
      ReadAbsoluteAnchor(ANode, AContainer)
    else
      DoError(sdxErrorInvalidAnchorDefinition, ssmtWarning);
  finally
    AContainer.EndUpdate;
  end;
end;

function TdxSpreadSheetXLSXReaderContainerParser.ReadPoint(ANode: TdxXMLNode; const XName, YName: TdxXMLString): TPoint;
begin
  try
    Result.X := dxEMUToPixels(StrToInt64(ANode.Attributes.GetValueAsString(XName)));
    Result.Y := dxEMUToPixels(StrToInt64(ANode.Attributes.GetValueAsString(YName)));
  except
    DoError(sdxErrorInvalidAnchorDefinition, ssmtWarning);
    Result := cxNullPoint;
  end;
end;

procedure TdxSpreadSheetXLSXReaderContainerParser.ReadAbsoluteAnchor(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
var
  AChildNode: TdxXMLNode;
begin
  AContainer.AnchorType := catAbsolute;
  if ANode.FindChild(sdxXLSXNodeXDRPos, AChildNode) then
    AContainer.AnchorPoint1.Offset := ReadPoint(AChildNode, sdxXLSXAttrCoordX, sdxXLSXAttrCoordY);
  if ANode.FindChild(sdxXLSXNodeXDRExt, AChildNode) then
    AContainer.AnchorPoint2.Offset := ReadPoint(AChildNode, sdxXLSXAttrCoordExtX, sdxXLSXAttrCoordExtY);
end;

procedure TdxSpreadSheetXLSXReaderContainerParser.ReadOneCellAnchor(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
var
  AChildNode: TdxXMLNode;
begin
  AContainer.AnchorType := catOneCell;
  if ANode.FindChild(sdxXLSXNodeAnchorFrom, AChildNode) then
    ReadAnchorMarker(AChildNode, AContainer.AnchorPoint1);
  if ANode.FindChild(sdxXLSXNodeXDRExt, AChildNode) then
    AContainer.AnchorPoint2.Offset := ReadPoint(AChildNode, sdxXLSXAttrCoordExtX, sdxXLSXAttrCoordExtY);
  ReadEditAsMode(ANode, AContainer);
end;

procedure TdxSpreadSheetXLSXReaderContainerParser.ReadTwoCellAnchor(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
var
  AChildNode: TdxXMLNode;
begin
  AContainer.AnchorType := catTwoCell;
  if ANode.FindChild(sdxXLSXNodeAnchorFrom, AChildNode) then
    ReadAnchorMarker(AChildNode, AContainer.AnchorPoint1);
  if ANode.FindChild(sdxXLSXNodeAnchorTo, AChildNode) then
    ReadAnchorMarker(AChildNode, AContainer.AnchorPoint2);
  ReadEditAsMode(ANode, AContainer);
end;

function TdxSpreadSheetXLSXReaderContainerParser.GetOwnerParser: TdxSpreadSheetXLSXReaderDrawingParser;
begin
  Result := inherited OwnerParser as TdxSpreadSheetXLSXReaderDrawingParser;
end;

procedure TdxSpreadSheetXLSXReaderContainerParser.ReadAnchorMarker(ANode: TdxXMLNode; AAnchor: TdxSpreadSheetContainerAnchorPoint);
var
  AColumnNode: TdxXMLNode;
  AOffset: TPoint;
  ARowNode: TdxXMLNode;
begin
  try
    if ANode.FindChild(sdxXLSXNodeDrawingPosMarkerRow, ARowNode) and ANode.FindChild(sdxXLSXNodeDrawingPosMarkerColumn, AColumnNode) then
      AAnchor.Cell := OwnerParser.View.CreateCell(StrToInt(ARowNode.TextAsString), StrToInt(AColumnNode.TextAsString))
    else
      AAnchor.Cell := nil;

    AOffset := cxNullPoint;
    if ANode.FindChild(sdxXLSXNodeDrawingPosMarkerColumnOffset, AColumnNode) then
      AOffset.X := dxEMUToPixels(StrToInt64(AColumnNode.TextAsString));
    if ANode.FindChild(sdxXLSXNodeDrawingPosMarkerRowOffset, ARowNode) then
      AOffset.Y := dxEMUToPixels(StrToInt64(ARowNode.TextAsString));
    AAnchor.Offset := AOffset;
  except
    DoError(sdxErrorInvalidAnchorDefinition, ssmtWarning);
  end;
end;

{ TdxSpreadSheetXLSXReaderDrawingShapeContainerParser }

function TdxSpreadSheetXLSXReaderDrawingShapeContainerParser.CreateContainer: TdxSpreadSheetContainer;
begin
  Result := OwnerParser.View.Containers.Add(TdxSpreadSheetShapeContainer);
end;

procedure TdxSpreadSheetXLSXReaderDrawingShapeContainerParser.ReadShapeFillParameters(
  ANode: TdxXMLNode; AShape: TdxSpreadSheetShape);
begin
  ExecuteSubTask(TdxSpreadSheetXLSXReaderBrushParser.Create(ANode, AShape.Brush, OwnerParser));
end;

procedure TdxSpreadSheetXLSXReaderDrawingShapeContainerParser.ReadContent(
  ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
var
  AChildNode: TdxXMLNode;
begin
  if ANode.FindChild([sdxXLSXNodeDrawingShapeContainer, sdxXLSXNodeDrawingStyle], AChildNode) then
    ReadShapeDefaultStyles(AChildNode, TdxSpreadSheetShapeContainer(AContainer).Shape);
  if ANode.FindChild([sdxXLSXNodeDrawingShapeContainer, sdxXLSXNodeDrawingShapeDescription, sdxXLSXNodeDrawingDescription], AChildNode) then
    ReadDescription(AChildNode, AContainer);
  if ANode.FindChild([sdxXLSXNodeDrawingShapeContainer, sdxXLSXNodeDrawingShapeProperties], AChildNode) then
    ReadShapeProperties(AChildNode, TdxSpreadSheetShapeContainer(AContainer));
  if ANode.FindChild([sdxXLSXNodeDrawingShapeContainer, sdxXLSXNodeDrawingShapeDescription,
    sdxXLSXNodeDrawingShapeAttributesEx, sdxXLSXNodeDrawingShapeLocks], AChildNode)
  then
    ReadContainerRestrictions(AChildNode, AContainer);
end;

procedure TdxSpreadSheetXLSXReaderDrawingShapeContainerParser.ReadShapeDefaultStyles(
  ANode: TdxXMLNode; AShape: TdxSpreadSheetShape);
var
  AChildNode: TdxXMLNode;
  AIndex: Integer;
begin
  if ANode.FindChild(sdxXLSXNodeLineRef, AChildNode) then
  begin
    AIndex := AChildNode.Attributes.GetValueAsInteger(sdxXLSXAttrRefIndex);
    if (AIndex >= 0) and (AIndex < Owner.ThemedPens.Count) then
      AShape.Pen.Assign(Owner.ThemedPens[AIndex]);
  end;

  if ANode.FindChild(sdxXLSXNodeFillRef, AChildNode) then
  begin
    AIndex := AChildNode.Attributes.GetValueAsInteger(sdxXLSXAttrRefIndex);
    if (AIndex >= 0) and (AIndex < Owner.ThemedBrushes.Count) then
      AShape.Brush.Assign(Owner.ThemedBrushes[AIndex]);
  end;
end;

procedure TdxSpreadSheetXLSXReaderDrawingShapeContainerParser.ReadShapeGeometry(ANode: TdxXMLNode; AShape: TdxSpreadSheetShape);
begin
  AShape.ShapeType := TdxSpreadSheetXLSXHelper.StringToShapeType(ANode.Attributes.GetValue(sdxXLSXAttrPreset));
end;

procedure TdxSpreadSheetXLSXReaderDrawingShapeContainerParser.ReadShapeLineParameters(ANode: TdxXMLNode; APen: TdxGPPen);
begin
  ExecuteSubTask(TdxSpreadSheetXLSXReaderPenParser.Create(ANode, APen, OwnerParser));
end;

procedure TdxSpreadSheetXLSXReaderDrawingShapeContainerParser.ReadShapeProperties(
  ANode: TdxXMLNode; AContainer: TdxSpreadSheetShapeContainer);
var
  AChildNode: TdxXMLNode;
begin
  if ANode.FindChild(sdxXLSXNodeDrawingXForm, AChildNode) then
    ReadTransformProperties(AChildNode, AContainer);

  if ANode.FindChild(sdxXLSXNodeDrawingShapeGeometry, AChildNode) then
    ReadShapeGeometry(AChildNode, AContainer.Shape);

  if ANode.FindChild(sdxXLSXNodeLine, AChildNode) then
    ReadShapeLineParameters(AChildNode, AContainer.Shape.Pen);

  if ANode.FindChild(sdxXLSXNodeSolidFill, AChildNode) or
     ANode.FindChild(sdxXLSXNodeTexturedFill, AChildNode) or
     ANode.FindChild(sdxXLSXNodeGradientFill, AChildNode) or
     ANode.FindChild(sdxXLSXNodePatternFill, AChildNode) or
     ANode.FindChild(sdxXLSXNodeNoFill, AChildNode)
  then
    ReadShapeFillParameters(AChildNode, AContainer.Shape);
end;

procedure TdxSpreadSheetXLSXReaderDrawingShapeContainerParser.ReadTransformProperties(
  ANode: TdxXMLNode; AContainer: TdxSpreadSheetShapeContainer);
begin
  AContainer.Transform.RotationAngle := TdxSpreadSheetXLSXUtils.DecodePositiveFixedAngle(
    ANode.Attributes.GetValueAsInteger(sdxXLSXAttrRot));
  AContainer.Transform.FlipHorizontally := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrFlipH);
  AContainer.Transform.FlipVertically := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrFlipV);
end;

{ TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser }

function TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser.CreateContainer: TdxSpreadSheetContainer;
begin
  Result := OwnerParser.View.Containers.Add(TdxSpreadSheetTextBoxContainer);
end;

procedure TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser.ReadContent(
  ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
var
  AChildNode: TdxXMLNode;
begin
  inherited ReadContent(ANode, AContainer);
  if ANode.FindChild([sdxXLSXNodeDrawingShapeContainer, sdxXLSXNodeDrawingTextBody], AChildNode) then
    ReadTextBox(AChildNode, TdxSpreadSheetTextBoxContainer(AContainer).TextBox);
end;

procedure TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser.ReadTextBox(
  ANode: TdxXMLNode; ATextBox: TdxSpreadSheetTextBox);
var
  ASubNode: TdxXMLNode;
  ATextNode: TdxXMLNode;
begin
  if ANode.FindChild(sdxXLSXNodeParagraph, ATextNode) then
  begin
    ATextBox.Text := TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextParser.Parse(Self, ATextNode);
    if ATextNode.FindChild(ATextNode.NameScope + sdxXLSXNodeRichTextParagraphProperties, ASubNode) then
      ATextBox.AlignHorz := TdxSpreadSheetXLSXHelper.StringToAlignment(ASubNode.Attributes.GetValue(sdxXLSXAttrAlign));
  end;

  if ANode.FindChild(sdxXLSXNodeBodyProperties, ASubNode) then
  begin
    ATextBox.AutoSize := ASubNode.FindChild(ASubNode.NameScope + sdxXLSXNodeSpAutoFit, ATextNode);
    ATextBox.AlignVert := TdxSpreadSheetXLSXHelper.StringToVerticalAlignment(ASubNode.Attributes.GetValue(sdxXLSXAttrAnchor));
    ATextBox.ContentOffsets := Rect(
      dxEMUToPixels(ASubNode.Attributes.GetValueAsInt64(sdxXLSXAttrLeftInset, dxXLSXDefaultTextPadding.Left)),
      dxEMUToPixels(ASubNode.Attributes.GetValueAsInt64(sdxXLSXAttrTopInset, dxXLSXDefaultTextPadding.Top)),
      dxEMUToPixels(ASubNode.Attributes.GetValueAsInt64(sdxXLSXAttrRightInset, dxXLSXDefaultTextPadding.Right)),
      dxEMUToPixels(ASubNode.Attributes.GetValueAsInt64(sdxXLSXAttrBottomInset, dxXLSXDefaultTextPadding.Bottom)));
    ATextBox.WordWrap := ASubNode.Attributes.GetValue(sdxXLSXAttrWrap) = sdxXLSXValueSquare;
  end;

  if ATextBox.Text is TdxSpreadSheetFormattedSharedString then
  begin
    if TdxSpreadSheetFormattedSharedString(ATextBox.Text).Runs.Count > 0 then
      ATextBox.Font.Handle := TdxSpreadSheetFormattedSharedString(ATextBox.Text).Runs[0].FontHandle;
  end;
end;

function TdxSpreadSheetXLSXReaderDrawingPictureContainerParser.CreateContainer: TdxSpreadSheetContainer;
begin
  Result := OwnerParser.View.Containers.Add(TdxSpreadSheetPictureContainer);
end;

procedure TdxSpreadSheetXLSXReaderDrawingPictureContainerParser.ReadContent(
  ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer);
var
  AChildNode: TdxXMLNode;
begin
  if ANode.FindChild([sdxXLSXNodeDrawingPictureContainer, sdxXLSXNodeDrawingPictureDescription], AChildNode) then
    ReadPictureContainerDescription(AChildNode, TdxSpreadSheetPictureContainer(AContainer));
  if ANode.FindChild([sdxXLSXNodeDrawingPictureContainer, sdxXLSXNodeDrawingBlipFill], AChildNode) then
    ReadPicture(AChildNode, TdxSpreadSheetPictureContainer(AContainer).Picture);
  if ANode.FindChild([sdxXLSXNodeDrawingPictureContainer, sdxXLSXNodeDrawingShapeProperties], AChildNode) then
    ReadShapeProperties(AChildNode, TdxSpreadSheetPictureContainer(AContainer));
end;

procedure TdxSpreadSheetXLSXReaderDrawingPictureContainerParser.ReadPicture(ANode: TdxXMLNode; APicture: TdxSpreadSheetPicture);
var
  AChildNode: TdxXMLNode;
begin
  if ANode.FindChild(sdxXLSXNodeDrawingBlip, AChildNode) then
    ReadPictureResource(AChildNode, APicture);
  if ANode.FindChild(sdxXLSXNodeDrawingAttributeSourceRect, AChildNode) and (AChildNode.Attributes.Count > 0) then
  begin
    APicture.CropMargins := TdxSpreadSheetXLSXUtils.DecodeSourceRect(cxRect(
      AChildNode.Attributes.GetValueAsInteger('l'), AChildNode.Attributes.GetValueAsInteger('t'),
      AChildNode.Attributes.GetValueAsInteger('r'), AChildNode.Attributes.GetValueAsInteger('b')));
  end;
end;

procedure TdxSpreadSheetXLSXReaderDrawingPictureContainerParser.ReadPictureContainerProperties(ANode: TdxXMLNode;
  AContainer: TdxSpreadSheetPictureContainer);
var
  AAttr: TdxXMLNodeAttribute;
begin
  if ANode.Attributes.Find(sdxXLSXAttrPreferRelativeResize, AAttr) then
    AContainer.RelativeResize := AAttr.ValueAsBoolean;
end;

procedure TdxSpreadSheetXLSXReaderDrawingPictureContainerParser.ReadPictureContainerDescription(
  ANode: TdxXMLNode; AContainer: TdxSpreadSheetPictureContainer);
var
  AChildNode: TdxXMLNode;
begin
  if ANode.FindChild(sdxXLSXNodeDrawingDescription, AChildNode) then
    ReadDescription(AChildNode, AContainer);
  if ANode.FindChild(sdxXLSXNodeDrawingPictureAttributes, AChildNode) then
    ReadPictureContainerProperties(AChildNode, AContainer);
  if ANode.FindChild([sdxXLSXNodeDrawingPictureAttributes, sdxXLSXNodeDrawingPictureLocks], AChildNode) then
    ReadContainerRestrictions(AChildNode, AContainer);
end;

procedure TdxSpreadSheetXLSXReaderDrawingPictureContainerParser.ReadPictureResource(
  ANode: TdxXMLNode; APicture: TdxSpreadSheetPicture);
var
  AStream: TStream;
begin
  if ReadImage(ANode, AStream) then
  try
    TdxSpreadSheetPictureAccess(APicture).ImageHandle := TdxSpreadSheetCustomReaderAccess(Owner).AddImage(AStream);
  finally
    AStream.Free;
  end;
end;

{ TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextParser }

constructor TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextParser.Create(
  AOwner: TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser);
begin
  FOwner := AOwner;
  inherited Create(AOwner.Owner);
end;

function TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextParser.CreateFontParser(
  AFont: TdxSpreadSheetFontHandle; ANode: TdxXMLNode): TdxSpreadSheetXLSXReaderFontParser;
begin
  Result := TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextFontParser.Create(AFont, ANode, Self);
end;

class function TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextParser.Parse(
  AOwner: TdxSpreadSheetXLSXReaderDrawingTextBoxContainerParser; ANode: TdxXMLNode): TdxSpreadSheetFormattedSharedString;
begin
  with Create(AOwner) do
  try
    Multiline := True;
    Result := DoParse(ANode);
  finally
    Free;
  end;
end;

{ TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextFontParser }

constructor TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextFontParser.Create(
  AFont: TdxSpreadSheetFontHandle; ANode: TdxXMLNode; AOwner: TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextParser);
begin
  FOwner := AOwner;
  inherited Create(AFont, ANode, AOwner.Owner.Owner);
end;

procedure TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextFontParser.Execute;
var
  AAttr: TdxXMLNodeAttribute;
  AStyle: TFontStyle;
  ASubNode: TdxXMLNode;
begin
  if Node.FindChild(sdxXLSXNodeSolidFill, ASubNode) then
    ReadFontColor(ASubNode);
  if Node.Attributes.Find(sdxXLSXNodeSZ, AAttr) then
    Font.Size := AAttr.ValueAsInt64 div 100;
  if Node.Attributes.Find(dxXLSXFontStyles[fsUnderline], AAttr) and (AAttr.Value <> 'none') then
    Font.Style := Font.Style + [fsUnderline];
  if Node.FindChild(sdxXLSXNodeLatin, ASubNode) and ASubNode.Attributes.Find(sdxXLSXAttrTypeface, AAttr) then
    Font.Name := AAttr.ValueAsString;
  for AStyle := Low(AStyle) to High(AStyle) do
    if Node.Attributes.GetValueAsBoolean(dxXLSXFontStyles[AStyle]) then
    begin
      Font.Style := Font.Style + [AStyle];
      Break;
    end;
end;

procedure TdxSpreadSheetXLSXReaderDrawingTextBoxContainerRichTextFontParser.ReadFontColor(ANode: TdxXMLNode);
var
  ABrush: TdxGPBrush;
begin
  ABrush := TdxGPBrush.Create;
  try
    ExecuteSubTask(TdxSpreadSheetXLSXReaderBrushParser.Create(ANode, ABrush, Owner.Owner.OwnerParser));
    Font.Color := dxAlphaColorToColor(ABrush.Color);
  finally
    ABrush.Free;
  end;
end;

end.
