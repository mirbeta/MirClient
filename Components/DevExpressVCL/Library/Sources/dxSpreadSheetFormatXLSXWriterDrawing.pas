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

unit dxSpreadSheetFormatXLSXWriterDrawing;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Graphics, dxCore, dxCoreClasses, cxClasses, dxCustomTree, dxXMLDoc, dxZIPUtils,
  dxSpreadSheetCore, dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetStrs, dxSpreadSheetPackedFileFormatCore,
  dxSpreadSheetUtils, dxSpreadSheetGraphics, Generics.Defaults, Generics.Collections, dxGDIPlusClasses, dxCoreGraphics, cxGeometry,
  dxSpreadSheetPrinting, dxSpreadSheetConditionalFormattingRules, dxSpreadSheetContainers, dxSpreadSheetFormatXLSXWriter,
  dxSpreadSheetHyperlinks, dxSpreadSheetCoreStyles;

type

  { TdxSpreadSheetXLSXWriterCustomContainerBuilder }

  TdxSpreadSheetXLSXWriterCustomContainerBuilderClass = class of TdxSpreadSheetXLSXWriterCustomContainerBuilder;
  TdxSpreadSheetXLSXWriterCustomContainerBuilder = class(TdxSpreadSheetXLSXWriterCustomBuilder)
  strict private
    FContainer: TdxSpreadSheetContainer;
    FParentNode: TdxXMLNode;

    procedure WriteAnchorMarker(ANode: TdxXMLNode; AAnchor: TdxSpreadSheetContainerAnchorPoint);
    procedure WriteEditAsMode(ANode: TdxXMLNode);
    procedure WritePoint(ANode: TdxXMLNode; const AValue: TPoint; const XName, YName: TdxXMLString);
  protected
    // Anchors
    function WriteAbsoluteAnchors(ANode: TdxXMLNode): TdxXMLNode; virtual;
    function WriteAnchors(ANode: TdxXMLNode): TdxXMLNode; virtual;
    function WriteOneCellAnchors(ANode: TdxXMLNode): TdxXMLNode; virtual;
    function WriteTwoCellAnchors(ANode: TdxXMLNode): TdxXMLNode; virtual;

    procedure WriteContent(ARootNode: TdxXMLNode); virtual;
  public
    constructor Create(AContainer: TdxSpreadSheetContainer;
      AOwner: TdxSpreadSheetXLSXWriter; AOwnerRels: TdxSpreadSheetXLSXWriterRels; AParentNode: TdxXMLNode);
    procedure Execute; override;
    //
    property Container: TdxSpreadSheetContainer read FContainer;
    property ParentNode: TdxXMLNode read FParentNode;
  end;

  { TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder }

  TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder = class(TdxSpreadSheetXLSXWriterCustomContainerBuilder)
  strict private
    function GetContainer: TdxSpreadSheetShapeContainer; inline;
  protected
    // Brushes
    procedure WriteColor(ANode: TdxXMLNode; AAlphaColor: TdxAlphaColor); virtual;
    procedure WriteGradientBrushParameters(ANode: TdxXMLNode; ABrush: TdxGPBrush); virtual;
    procedure WriteSolidBrushParameters(ANode: TdxXMLNode; ABrush: TdxGPBrush); virtual;
    procedure WriteTextureBrushParameters(ANode: TdxXMLNode; ABrush: TdxGPBrush); virtual;

    procedure WriteDescription(ANode: TdxXMLNode); virtual;
    procedure WriteHyperlink(ANode: TdxXMLNode; AHyperlink: TdxSpreadSheetHyperlink); virtual;
    procedure WriteRestrictions(ANode: TdxXMLNode); virtual;
    procedure WriteShapeFillParameters(AParentNode: TdxXMLNode; ABrush: TdxGPBrush); virtual;
    procedure WriteShapeGeometry(ANode: TdxXMLNode); virtual;
    procedure WriteShapeLineParameters(ANode: TdxXMLNode; APen: TdxGPPen); virtual;
    procedure WriteShapeProperties(ANode: TdxXMLNode); virtual;
    procedure WriteTransform(ANode: TdxXMLNode);
  public
    property Container: TdxSpreadSheetShapeContainer read GetContainer;
  end;

  { TdxSpreadSheetXLSXWriterShapeContainerBuilder }

  TdxSpreadSheetXLSXWriterShapeContainerBuilder = class(TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder)
  protected
    procedure WriteContent(ARootNode: TdxXMLNode); override;
    procedure WriteShapeDescription(ANode: TdxXMLNode); virtual;
  end;

  { TdxSpreadSheetXLSXWriterTextBoxContainerBuilder }

  TdxSpreadSheetXLSXWriterTextBoxContainerBuilder = class(TdxSpreadSheetXLSXWriterShapeContainerBuilder)
  strict private
    function GetContainer: TdxSpreadSheetTextBoxContainer; inline;
  protected
    procedure WriteContent(ARootNode: TdxXMLNode); override;
    procedure WriteTextBoxBody(ANode: TdxXMLNode; ATextBox: TdxSpreadSheetTextBox);
    procedure WriteTextBoxBodyData(ANode: TdxXMLNode; ATextBox: TdxSpreadSheetTextBox);
    procedure WriteTextBoxBodyParagraphInfo(ANode: TdxXMLNode; AFont: TdxSpreadSheetFontHandle);
    procedure WriteTextBoxBodyProperties(ANode: TdxXMLNode; ATextBox: TdxSpreadSheetTextBox);
  public
    property Container: TdxSpreadSheetTextBoxContainer read GetContainer;
  end;

  { TdxSpreadSheetXLSXWriterPictureContainerBuilder }

  TdxSpreadSheetXLSXWriterPictureContainerBuilder = class(TdxSpreadSheetXLSXWriterShapeContainerBuilder)
  strict private
    function GetContainer: TdxSpreadSheetPictureContainer; inline;
  protected
    procedure WriteContent(ARootNode: TdxXMLNode); override;
    procedure WritePictureDescription(ANode: TdxXMLNode); virtual;
    procedure WritePictureResource(ANode: TdxXMLNode); virtual;
  public
    property Container: TdxSpreadSheetPictureContainer read GetContainer;
  end;

  { TdxSpreadSheetXLSXWriterWorksheetContainersBuilder }

  TdxSpreadSheetXLSXWriterWorksheetContainersBuilder = class(TdxSpreadSheetXLSXWriterWorksheetTableViewSubFileBuilder)
  strict private
    FContainerBuildersMap: TDictionary<TClass, TdxSpreadSheetXLSXWriterCustomContainerBuilderClass>;
  protected
    function GetContainerBuilderClass(AContainer: TdxSpreadSheetContainer): TdxSpreadSheetXLSXWriterCustomContainerBuilderClass;
    procedure WriteContainer(ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
    procedure WriteContainers(ANode: TdxXMLNode; ARels: TdxSpreadSheetXLSXWriterRels); virtual;
  public
    constructor Create(const ATargetFileName: AnsiString; AOwnerRels: TdxSpreadSheetXLSXWriterRels;
      AOwnerWriter: TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder);
    destructor Destroy; override;
    procedure Execute; override;
    //
    property ContainerBuildersMap: TDictionary<TClass, TdxSpreadSheetXLSXWriterCustomContainerBuilderClass> read FContainerBuildersMap;
  end;

implementation

uses
  AnsiStrings, Math, TypInfo, StrUtils, dxColorPicker, cxGraphics, dxHashUtils, dxTypeHelpers,
  dxSpreadSheetFormulas, dxSpreadSheetFormatXLSXTags, dxSpreadSheetFormatXLSX, dxSpreadSheetFormatUtils,
  dxSpreadSheetConditionalFormattingIconSet, dxSpreadSheetCoreStrs;

const
  sMsgWrongDataType = 'wrong data type';

type
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  TdxSpreadSheetRelsWriterAccess = class(TdxSpreadSheetXLSXWriterRels);

{ TdxSpreadSheetXLSXWriterCustomContainerBuilder }

constructor TdxSpreadSheetXLSXWriterCustomContainerBuilder.Create(AContainer: TdxSpreadSheetContainer;
  AOwner: TdxSpreadSheetXLSXWriter; AOwnerRels: TdxSpreadSheetXLSXWriterRels; AParentNode: TdxXMLNode);
begin
  inherited Create(AOwner, AOwnerRels);
  FParentNode := AParentNode;
  FContainer := AContainer;
end;

procedure TdxSpreadSheetXLSXWriterCustomContainerBuilder.Execute;
begin
  WriteContent(WriteAnchors(ParentNode));
end;

function TdxSpreadSheetXLSXWriterCustomContainerBuilder.WriteAnchors(ANode: TdxXMLNode): TdxXMLNode;
begin
  Result := ANode;
  case Container.AnchorType of
    catAbsolute:
      Result := WriteAbsoluteAnchors(ANode);
    catOneCell:
      Result := WriteOneCellAnchors(ANode);
    catTwoCell:
      Result := WriteTwoCellAnchors(ANode);
  else
    DoError(sdxErrorInternal, [sMsgWrongDataType], ssmtError);
  end;
end;

function TdxSpreadSheetXLSXWriterCustomContainerBuilder.WriteAbsoluteAnchors(ANode: TdxXMLNode): TdxXMLNode;
begin
  Result := ANode.AddChild(sdxXLSXNodeAnchorAbsolute);
  WritePoint(Result.AddChild(sdxXLSXNodeXDRPos), Container.AnchorPoint1.Offset, sdxXLSXAttrCoordX, sdxXLSXAttrCoordY);
  WritePoint(Result.AddChild(sdxXLSXNodeXDRExt), Container.AnchorPoint2.Offset, sdxXLSXAttrCoordExtX, sdxXLSXAttrCoordExtY);
end;

function TdxSpreadSheetXLSXWriterCustomContainerBuilder.WriteOneCellAnchors(ANode: TdxXMLNode): TdxXMLNode;
begin
  Result := ANode.AddChild(sdxXLSXNodeAnchorOneCell);
  WriteAnchorMarker(Result.AddChild(sdxXLSXNodeAnchorFrom), Container.AnchorPoint1);
  WritePoint(Result.AddChild(sdxXLSXNodeXDRExt), Container.AnchorPoint2.Offset, sdxXLSXAttrCoordExtX, sdxXLSXAttrCoordExtY);
end;

function TdxSpreadSheetXLSXWriterCustomContainerBuilder.WriteTwoCellAnchors(ANode: TdxXMLNode): TdxXMLNode;
begin
  Result := ANode.AddChild(sdxXLSXNodeAnchorTwoCell);
  WriteAnchorMarker(Result.AddChild(sdxXLSXNodeAnchorFrom), Container.AnchorPoint1);
  WriteAnchorMarker(Result.AddChild(sdxXLSXNodeAnchorTo), Container.AnchorPoint2);
  WriteEditAsMode(Result);
end;

procedure TdxSpreadSheetXLSXWriterCustomContainerBuilder.WriteContent(ARootNode: TdxXMLNode);
begin
  // do nothing
end;

procedure TdxSpreadSheetXLSXWriterCustomContainerBuilder.WriteAnchorMarker(ANode: TdxXMLNode; AAnchor: TdxSpreadSheetContainerAnchorPoint);

  function GetCellColumnIndex(AValue: TdxSpreadSheetCell): Integer;
  begin
    if AValue <> nil then
      Result := AValue.ColumnIndex
    else
      Result := 0;
  end;

  function GetCellRowIndex(AValue: TdxSpreadSheetCell): Integer;
  begin
    if AValue <> nil then
      Result := AValue.RowIndex
    else
      Result := 0;
  end;

begin
  ANode.AddChild(sdxXLSXNodeDrawingPosMarkerColumn).TextAsString := IntToStr(GetCellColumnIndex(AAnchor.Cell));
  ANode.AddChild(sdxXLSXNodeDrawingPosMarkerColumnOffset).TextAsString := IntToStr(dxPixelsToEMU(AAnchor.Offset.X));

  ANode.AddChild(sdxXLSXNodeDrawingPosMarkerRow).TextAsString := IntToStr(GetCellRowIndex(AAnchor.Cell));
  ANode.AddChild(sdxXLSXNodeDrawingPosMarkerRowOffset).TextAsString := IntToStr(dxPixelsToEMU(AAnchor.Offset.Y));
end;

procedure TdxSpreadSheetXLSXWriterCustomContainerBuilder.WriteEditAsMode(ANode: TdxXMLNode);
begin
  if not Container.AnchorPoint1.FixedToCell then
    ANode.Attributes.SetValue(sdxXLSXAttrEditAs, sdxXLSXValueEditAsAbsolute)
  else
    if Container.AnchorPoint2.FixedToCell then
      ANode.Attributes.SetValue(sdxXLSXAttrEditAs, sdxXLSXValueEditAsTwoCell)
    else
      ANode.Attributes.SetValue(sdxXLSXAttrEditAs, sdxXLSXValueEditAsOneCell);
end;

procedure TdxSpreadSheetXLSXWriterCustomContainerBuilder.WritePoint(
  ANode: TdxXMLNode; const AValue: TPoint; const XName, YName: TdxXMLString);
begin
  ANode.Attributes.SetValueAsInt64(XName, dxPixelsToEMU(AValue.X));
  ANode.Attributes.SetValueAsInt64(YName, dxPixelsToEMU(AValue.Y));
end;

{ TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder }

procedure TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.WriteColor(ANode: TdxXMLNode; AAlphaColor: TdxAlphaColor);
var
  AAlpha: Byte;
begin
  ANode.Attributes.Add(sdxXLSXAttrVal, TdxColorHelper.AlphaColorToHexCode(AAlphaColor, False, False));

  AAlpha := dxGetAlpha(AAlphaColor);
  if AAlpha <> MaxByte then
    ANode.AddChild(sdxXLSXNodeColorAlpha).Attributes.Add(sdxXLSXAttrVal, TdxSpreadSheetXLSXUtils.EncodeColorAlpha(AAlpha));
end;

procedure TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.WriteGradientBrushParameters(ANode: TdxXMLNode; ABrush: TdxGPBrush);

  procedure WriteGradientPoint(AChildNode: TdxXMLNode; AOffset: Single; AColor: TdxAlphaColor);
  var
    AGradientPointNode: TdxXMLNode;
  begin
    AGradientPointNode := AChildNode.AddChild(sdxXLSXNodeGradientPoint);
    AGradientPointNode.Attributes.Add(sdxXLSXAttrGradientPointPos, TdxSpreadSheetXLSXUtils.EncodePercents(AOffset) * 100);
    WriteColor(AGradientPointNode.AddChild(sdxXLSXNodeThemesCustomColor), AColor);
  end;

const
  AnglesMap: array[TdxGPBrushGradientMode] of Integer = (0, 90, 45, 135);
var
  AChildNode: TdxXMLNode;
  I: Integer;
begin
  if ABrush.GradientPoints.Count = 0 then
    Exit;

  AChildNode := ANode.AddChild(sdxXLSXNodeGradientPoints);
  if not SameValue(ABrush.GradientPoints.Offsets[0], 0) then
    WriteGradientPoint(AChildNode, 0, 0);
  for I := 0 to ABrush.GradientPoints.Count - 1 do
    WriteGradientPoint(AChildNode, ABrush.GradientPoints.Offsets[I], ABrush.GradientPoints.Colors[I]);
  if not SameValue(ABrush.GradientPoints.Offsets[ABrush.GradientPoints.Count - 1], 1) then
    WriteGradientPoint(AChildNode, 1, 0);

  AChildNode := ANode.AddChild(sdxXLSXNodeLinearGradientFill);
  AChildNode.Attributes.Add(sdxXLSXAttrAng, TdxSpreadSheetXLSXUtils.EncodePositiveFixedAngle(AnglesMap[ABrush.GradientMode]));
  AChildNode.Attributes.Add(sdxXLSXAttrScaled, True);
end;

procedure TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.WriteSolidBrushParameters(ANode: TdxXMLNode; ABrush: TdxGPBrush);
begin
  WriteColor(ANode.AddChild(sdxXLSXNodeThemesCustomColor), ABrush.Color);
end;

procedure TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.WriteTextureBrushParameters(ANode: TdxXMLNode; ABrush: TdxGPBrush);
var
  AChildNode: TdxXMLNode;
begin
  ANode.Attributes.SetValueAsBoolean(sdxXLSXAttrRotateWithShape, True);

  AChildNode := ANode.AddChild(sdxXLSXNodeDrawingBlip);
  if not ABrush.Texture.Empty then
  begin
    AChildNode.Attributes.SetValue(sdxXLSXAttrXMLNSR, sdxXLSXCommonRelationshipPath);
    AChildNode.Attributes.SetValue(sdxXLSXAttrDrawingResourceEmbed, WriteImage(ABrush.Texture));
  end;

  ANode.AddChild(sdxXLSXNodeDrawingAttributeSourceRect);

  AChildNode := ANode.AddChild(sdxXLSXNodeTile);
  AChildNode.Attributes.SetValueAsInteger(sdxXLSXAttrTileTX, 0);
  AChildNode.Attributes.SetValueAsInteger(sdxXLSXAttrTileTY, 0);
  AChildNode.Attributes.SetValueAsInteger(sdxXLSXAttrTileSX, TdxSpreadSheetXLSXUtils.EncodePercents(100));
  AChildNode.Attributes.SetValueAsInteger(sdxXLSXAttrTileSY, TdxSpreadSheetXLSXUtils.EncodePercents(100));
  AChildNode.Attributes.SetValueAsString(sdxXLSXAttrFlip, 'none');
  AChildNode.Attributes.SetValueAsString(sdxXLSXAttrTileAlign, 'tl');
end;

procedure TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.WriteDescription(ANode: TdxXMLNode);
begin
  ANode.Attributes.SetValueAsInteger('id', Container.Index + 1);
  ANode.Attributes.SetValueAsString(sdxXLSXAttrName, Container.Name);
  if Container.Description <> '' then
    ANode.Attributes.SetValueAsString(sdxXLSXAttrAlternateText, Container.Description);
  if Container.Title <> '' then
    ANode.Attributes.SetValueAsString(sdxXLSXAttrTitle, Container.Title);
  if Container.Hyperlink <> nil then
    WriteHyperlink(ANode, Container.Hyperlink);
  if not Container.Visible then
    ANode.Attributes.SetValueAsBoolean(sdxXLSXAttrHidden, True);
end;

procedure TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.WriteHyperlink(
  ANode: TdxXMLNode; AHyperlink: TdxSpreadSheetHyperlink);
var
  AValue: string;
  ARelsNode: TdxXMLNode;
begin
  ANode := ANode.AddChild(sdxXLSXNodeDrawingShapeHLink);
  AValue := AHyperlink.Value;
  if (AHyperlink.ValueType = hvtReference) and (Pos('=', AValue) = 1) then
    AValue[1] := '#';
  if havScreenTip in AHyperlink.AssignedValues then
    ANode.Attributes.Add(sdxXLSXAttrTooltip, AHyperlink.ScreenTip);
  ARelsNode := TdxSpreadSheetRelsWriterAccess(OwnerRels).DoAddRelationship(sdxXLSXHyperlinkRelationship,
    dxStringToXMLString(AValue));
  if AHyperlink.ValueType <> hvtReference then
    ARelsNode.Attributes.Add(sdxXLSXAttrTargetMode, sdxXLSXValueTargetModeExternal);
  ANode.Attributes.SetValue(sdxXLSXAttrXMLNSR, sdxXLSXCommonRelationshipPath);
  ANode.Attributes.Add(sdxXLSXAttrRId, ARelsNode.Attributes.GetValue(sdxXLSXAttrId));
end;

procedure TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.WriteRestrictions(ANode: TdxXMLNode);
var
  AIndex: TdxSpreadSheetContainerRestriction;
begin
  for AIndex := Low(TdxSpreadSheetContainerRestriction) to High(TdxSpreadSheetContainerRestriction) do
  begin
    if AIndex in Container.Restrictions then
      ANode.Attributes.SetValueAsBoolean(dxXLSXRestrictionNames[AIndex], True);
  end;
end;

procedure TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.WriteShapeFillParameters(AParentNode: TdxXMLNode; ABrush: TdxGPBrush);
begin
  if ABrush.IsEmpty then
    AParentNode.AddChild(sdxXLSXNodeNoFill)
  else
    case ABrush.Style of
      gpbsSolid:
        WriteSolidBrushParameters(AParentNode.AddChild(sdxXLSXNodeSolidFill), ABrush);
      gpbsGradient:
        WriteGradientBrushParameters(AParentNode.AddChild(sdxXLSXNodeGradientFill), ABrush);
      gpbsTexture:
        WriteTextureBrushParameters(AParentNode.AddChild(sdxXLSXNodeTexturedFill), ABrush);
    end;
end;

procedure TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.WriteShapeGeometry(ANode: TdxXMLNode);
begin
  ANode.Attributes.Add(sdxXLSXAttrPreset, dxXLSXShapeTypeMap[Container.Shape.ShapeType]);
  ANode.AddChild(sdxXLSXNodeAVList);
end;

procedure TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.WriteShapeLineParameters(ANode: TdxXMLNode; APen: TdxGPPen);
begin
  ANode.Attributes.SetValueAsInt64(sdxXLSXAttrLineWidth, dxPixelsToEMUF(APen.Width));
  WriteShapeFillParameters(ANode, APen.Brush);
  if APen.Style <> gppsSolid then
    ANode.AddChild(sdxXLSXNodeLineDash).Attributes.Add(sdxXLSXAttrVal, dxXLSXPenStyleMap[APen.Style]);
end;

procedure TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.WriteShapeProperties(ANode: TdxXMLNode);
begin
  WriteTransform(ANode.AddChild(sdxXLSXNodeDrawingXForm));
  WriteShapeGeometry(ANode.AddChild(sdxXLSXNodeDrawingShapeGeometry));
  WriteShapeFillParameters(ANode, Container.Shape.Brush);
  WriteShapeLineParameters(ANode.AddChild(sdxXLSXNodeLine), Container.Shape.Pen);
end;

procedure TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.WriteTransform(ANode: TdxXMLNode);
var
  AAngle: Integer;
  ABounds: TRect;
  AChildNode: TdxXMLNode;
begin
  AAngle := Round(Container.Transform.RotationAngle);
  if AAngle <> 0 then
    ANode.Attributes.SetValueAsInteger(sdxXLSXAttrRot, TdxSpreadSheetXLSXUtils.EncodePositiveFixedAngle(AAngle));
  if Container.Transform.FlipHorizontally then
    ANode.Attributes.SetValueAsBoolean(sdxXLSXAttrFlipH, Container.Transform.FlipHorizontally);
  if Container.Transform.FlipVertically then
    ANode.Attributes.SetValueAsBoolean(sdxXLSXAttrFlipV, Container.Transform.FlipVertically);

  ABounds := TdxSpreadSheetContainerAccess(Container).Calculator.CalculateBounds;

  AChildNode := ANode.AddChild(sdxXLSXNodeCoordOff);
  AChildNode.Attributes.Add(sdxXLSXAttrCoordX, dxPixelsToEMU(ABounds.Left));
  AChildNode.Attributes.Add(sdxXLSXAttrCoordY, dxPixelsToEMU(ABounds.Top));

  AChildNode := ANode.AddChild(sdxXLSXNodeCoordExt);
  AChildNode.Attributes.Add(sdxXLSXAttrCoordExtX, dxPixelsToEMU(ABounds.Width));
  AChildNode.Attributes.Add(sdxXLSXAttrCoordExtY, dxPixelsToEMU(ABounds.Height));
end;

function TdxSpreadSheetXLSXWriterCustomShapeContainerBuilder.GetContainer: TdxSpreadSheetShapeContainer;
begin
  Result := inherited Container as TdxSpreadSheetShapeContainer;
end;

{ TdxSpreadSheetXLSXWriterShapeContainerBuilder }

procedure TdxSpreadSheetXLSXWriterShapeContainerBuilder.WriteContent(ARootNode: TdxXMLNode);
var
  ANode: TdxXMLNode;
begin
  ANode := ARootNode.AddChild(sdxXLSXNodeDrawingShapeContainer);
  ANode.Attributes.SetValue(sdxXLSXAttrMacro, '');
  ANode.Attributes.SetValue(sdxXLSXAttrTextLink, '');
  WriteShapeDescription(ANode.AddChild(sdxXLSXNodeDrawingShapeDescription));
  WriteShapeProperties(ANode.AddChild(sdxXLSXNodeDrawingShapeProperties));

  ARootNode.AddChild(sdxXLSXNodeClientData);
end;

procedure TdxSpreadSheetXLSXWriterShapeContainerBuilder.WriteShapeDescription(ANode: TdxXMLNode);
begin
  WriteDescription(ANode.AddChild(sdxXLSXNodeDrawingDescription));
  WriteRestrictions(ANode.AddChild(sdxXLSXNodeDrawingShapeAttributesEx).AddChild(sdxXLSXNodeDrawingShapeLocks));
end;

{ TdxSpreadSheetXLSXWriterTextBoxContainerBuilder }

procedure TdxSpreadSheetXLSXWriterTextBoxContainerBuilder.WriteContent(ARootNode: TdxXMLNode);
var
  ANode: TdxXMLNode;
  ASubNode: TdxXMLNode;
begin
  inherited WriteContent(ARootNode);
  ANode := ARootNode.FindChild(sdxXLSXNodeDrawingShapeContainer);
  if ANode.FindChild([sdxXLSXNodeDrawingShapeDescription, sdxXLSXNodeDrawingShapeAttributesEx], ASubNode) then
    ASubNode.Attributes.SetValueAsBoolean(sdxXLSXAttrTextBox, True);
  WriteTextBoxBody(ANode.AddChild(sdxXLSXNodeDrawingTextBody), Container.TextBox);
end;

procedure TdxSpreadSheetXLSXWriterTextBoxContainerBuilder.WriteTextBoxBody(
  ANode: TdxXMLNode; ATextBox: TdxSpreadSheetTextBox);
begin
  WriteTextBoxBodyProperties(ANode.AddChild(sdxXLSXNodeBodyProperties), ATextBox);
  ANode.AddChild(sdxXLSXNodeListStyle);
  WriteTextBoxBodyData(ANode.AddChild(sdxXLSXNodeParagraph), ATextBox);
end;

procedure TdxSpreadSheetXLSXWriterTextBoxContainerBuilder.WriteTextBoxBodyData(
  ANode: TdxXMLNode; ATextBox: TdxSpreadSheetTextBox);
var
  ARunNode: TdxXMLNode;
begin
  ANode.AddChild(ANode.NameScope + sdxXLSXNodeRichTextParagraphProperties).Attributes.SetValue(
    sdxXLSXAttrAlign, dxXLSXAlignmentMap[ATextBox.AlignHorz]);

  ARunNode := ANode.AddChild(ANode.NameScope + sdxXLSXNodeRichTextRun);
  WriteTextBoxBodyParagraphInfo(ARunNode.AddChild(ANode.NameScope + sdxXLSXNodeRichTextRunParagraph), ATextBox.Font.Handle);
  ARunNode.AddChild(ANode.NameScope + sdxXLSXNodeText).TextAsString := ATextBox.TextAsString;

  ANode.AddChild(ANode.NameScope + sdxXLSXNodeRichTextEndParagraphRunProperties);
end;

procedure TdxSpreadSheetXLSXWriterTextBoxContainerBuilder.WriteTextBoxBodyParagraphInfo(
  ANode: TdxXMLNode; AFont: TdxSpreadSheetFontHandle);
var
  AColorNode: TdxXMLNode;
begin
  ANode.Attributes.SetValueAsInteger(sdxXLSXNodeSZ, AFont.Size * 100);
  if fsUnderline in AFont.Style then
    ANode.Attributes.SetValue(dxXLSXFontStyles[fsUnderline], 'sng');
  if fsBold in AFont.Style then
    ANode.Attributes.SetValueAsBoolean(dxXLSXFontStyles[fsBold], True);
  if fsItalic in AFont.Style then
    ANode.Attributes.SetValueAsBoolean(dxXLSXFontStyles[fsItalic], True);
  if fsStrikeOut in AFont.Style then
    ANode.Attributes.SetValueAsBoolean(dxXLSXFontStyles[fsStrikeOut], True);

  AColorNode := ANode.AddChild(sdxXLSXNodeSolidFill).AddChild(sdxXLSXNodeThemesCustomColor);
  AColorNode.Attributes.SetValueAsString(sdxXLSXAttrVal, TdxColorHelper.AlphaColorToHexCode(
    dxColorToAlphaColor(cxGetActualColor(AFont.Color, clWindowText)), False, False));

  if SpreadSheet.DefaultCellStyle.Font.Name <> AFont.Name then
    ANode.AddChild(sdxXLSXNodeLatin).Attributes.SetValueAsString(sdxXLSXAttrTypeface, AFont.Name);
end;

procedure TdxSpreadSheetXLSXWriterTextBoxContainerBuilder.WriteTextBoxBodyProperties(
  ANode: TdxXMLNode; ATextBox: TdxSpreadSheetTextBox);
begin
  ANode.Attributes.SetValueAsInt64(sdxXLSXAttrLeftInset, dxPixelsToEMU(ATextBox.ContentOffsets.Left));
  ANode.Attributes.SetValueAsInt64(sdxXLSXAttrTopInset, dxPixelsToEMU(ATextBox.ContentOffsets.Top));
  ANode.Attributes.SetValueAsInt64(sdxXLSXAttrRightInset, dxPixelsToEMU(ATextBox.ContentOffsets.Right));
  ANode.Attributes.SetValueAsInt64(sdxXLSXAttrBottomInset, dxPixelsToEMU(ATextBox.ContentOffsets.Bottom));
  ANode.Attributes.SetValue(sdxXLSXAttrAnchor, dxXLSXVerticalAlignmentMap[ATextBox.AlignVert]);
  ANode.Attributes.SetValue('horzOverflow', 'clip');
  ANode.Attributes.SetValue('vertOverflow', 'clip');

  if ATextBox.WordWrap then
    ANode.Attributes.SetValue(sdxXLSXAttrWrap, sdxXLSXValueSquare)
  else
    ANode.Attributes.SetValue(sdxXLSXAttrWrap, sdxXLSXValueNone);

  if ATextBox.AutoSize then
    ANode.AddChild(ANode.NameScope + sdxXLSXNodeSpAutoFit)
  else
    ANode.AddChild(ANode.NameScope + sdxXLSXNodeNoAutoFit);
end;

function TdxSpreadSheetXLSXWriterTextBoxContainerBuilder.GetContainer: TdxSpreadSheetTextBoxContainer;
begin
  Result := inherited Container as TdxSpreadSheetTextBoxContainer;
end;

{ TdxSpreadSheetXLSXWriterPictureContainerBuilder }

procedure TdxSpreadSheetXLSXWriterPictureContainerBuilder.WriteContent(ARootNode: TdxXMLNode);
var
  ANode: TdxXMLNode;
begin
  ANode := ARootNode.AddChild(sdxXLSXNodeDrawingPictureContainer);
  WritePictureDescription(ANode.AddChild(sdxXLSXNodeDrawingPictureDescription));
  WritePictureResource(ANode.AddChild(sdxXLSXNodeDrawingBlipFill));
  WriteShapeProperties(ANode.AddChild(sdxXLSXNodeDrawingShapeProperties));
  ARootNode.AddChild(sdxXLSXNodeClientData);
end;

procedure TdxSpreadSheetXLSXWriterPictureContainerBuilder.WritePictureDescription(ANode: TdxXMLNode);
begin
  WriteDescription(ANode.AddChild(sdxXLSXNodeDrawingDescription));
  ANode := ANode.AddChild(sdxXLSXNodeDrawingPictureAttributes);
  if not Container.RelativeResize then
    ANode.Attributes.SetValueAsBoolean(sdxXLSXAttrPreferRelativeResize, False);
  WriteRestrictions(ANode.AddChild(sdxXLSXNodeDrawingPictureLocks));
end;

procedure TdxSpreadSheetXLSXWriterPictureContainerBuilder.WritePictureResource(ANode: TdxXMLNode);
var
  AChildNode: TdxXMLNode;
begin
  AChildNode := ANode.AddChild(sdxXLSXNodeDrawingBlip);
  if not Container.Picture.Empty then
  begin
    AChildNode.Attributes.SetValue(sdxXLSXAttrXMLNSR, sdxXLSXCommonRelationshipPath);
    AChildNode.Attributes.SetValue(sdxXLSXAttrDrawingResourceEmbed, WriteImage(Container.Picture.Image));
  end;
  ANode.AddChild(sdxXLSXNodeStretch).AddChild(sdxXLSXNodeFillRect);
end;

function TdxSpreadSheetXLSXWriterPictureContainerBuilder.GetContainer: TdxSpreadSheetPictureContainer;
begin
  Result := inherited Container as TdxSpreadSheetPictureContainer;
end;

{ TdxSpreadSheetXLSXWriterWorksheetContainersBuilder }

constructor TdxSpreadSheetXLSXWriterWorksheetContainersBuilder.Create(const ATargetFileName: AnsiString;
  AOwnerRels: TdxSpreadSheetXLSXWriterRels; AOwnerWriter: TdxSpreadSheetXLSXWriterWorksheetTableViewBuilder);
begin
  inherited Create(ATargetFileName, AOwnerRels, AOwnerWriter);
  FContainerBuildersMap := TDictionary<TClass, TdxSpreadSheetXLSXWriterCustomContainerBuilderClass>.Create;
  FContainerBuildersMap.Add(TdxSpreadSheetPictureContainer, TdxSpreadSheetXLSXWriterPictureContainerBuilder);
  FContainerBuildersMap.Add(TdxSpreadSheetShapeContainer, TdxSpreadSheetXLSXWriterShapeContainerBuilder);
  FContainerBuildersMap.Add(TdxSpreadSheetTextBoxContainer, TdxSpreadSheetXLSXWriterTextBoxContainerBuilder);
end;

destructor TdxSpreadSheetXLSXWriterWorksheetContainersBuilder.Destroy;
begin
  FreeAndNil(FContainerBuildersMap);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetContainersBuilder.Execute;
var
  ADoc: TdxXMLDocument;
  ANode: TdxXMLNode;
  ARels: TdxSpreadSheetXLSXWriterRels;
begin
  ADoc := TdxSpreadSheetXMLDocument.Create(nil);
  ARels := TdxSpreadSheetXLSXWriterRels.Create(nil);
  try
    ANode := ADoc.Root.AddChild(sdxXLSXNodeDrawingHeader);
    ANode.Attributes.SetValue(sdxXLSXAttrXMLNSXDR, sdxXLSXDrawingNamespaceXDR);
    ANode.Attributes.SetValue(sdxXLSXAttrXMLNSA, sdxXLSXDrawingNamespace);
    WriteContainers(ANode, ARels);

    RegisterFile(TargetFileName, sdxXLSXDrawingContentType, sdxXLSXDrawingRelationship, OwnerRels);
    WriteXML(TargetFileNameRels, ARels);
    WriteXML(TargetFileName, ADoc);
  finally
    ARels.Free;
    ADoc.Free;
  end;
end;

function TdxSpreadSheetXLSXWriterWorksheetContainersBuilder.GetContainerBuilderClass(
  AContainer: TdxSpreadSheetContainer): TdxSpreadSheetXLSXWriterCustomContainerBuilderClass;
begin
  Result := ContainerBuildersMap[AContainer.ClassType];
  if Result = nil then
    Result := TdxSpreadSheetXLSXWriterCustomContainerBuilder;
end;

procedure TdxSpreadSheetXLSXWriterWorksheetContainersBuilder.WriteContainer(
  ANode: TdxXMLNode; AContainer: TdxSpreadSheetContainer; ARels: TdxSpreadSheetXLSXWriterRels);
begin
  ExecuteSubTask(GetContainerBuilderClass(AContainer).Create(AContainer, Owner, ARels, ANode));
end;

procedure TdxSpreadSheetXLSXWriterWorksheetContainersBuilder.WriteContainers(
  ANode: TdxXMLNode; ARels: TdxSpreadSheetXLSXWriterRels);
var
  I: Integer;
begin
  for I := 0 to OwnerWriter.Containers.Count - 1 do
    WriteContainer(ANode, OwnerWriter.Containers[I], ARels);
end;

end.
