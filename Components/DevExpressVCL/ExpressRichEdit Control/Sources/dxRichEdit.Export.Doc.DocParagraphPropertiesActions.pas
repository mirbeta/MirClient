{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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
unit dxRichEdit.Export.Doc.DocParagraphPropertiesActions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxCoreGraphics,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DocStyles,
  dxRichEdit.Import.Doc.DocStyleSheet,
  dxRichEdit.Import.Doc.DocPictureBulletInformation;

type
  { TdxDocParagraphPropertiesActions }

  TdxDocParagraphPropertiesActions = class
  strict private
    FWriter: TBinaryWriter;
    FParagraphFormattingInfo: TdxParagraphFormattingInfo;
    FParagraphFormattingOptions: TdxParagraphFormattingOptions;
    FTabs: TdxTabFormattingInfo;
    FNumberingListIndex: TdxNumberingListIndex;
    FListLevelIndex: Integer;
    FUnitConverter: TdxDocumentModelUnitConverter;
    FFrameProperties: TdxFrameProperties;
    FDocumentModel: TdxDocumentModel;
  protected
    property ParagraphFormattingInfo: TdxParagraphFormattingInfo read FParagraphFormattingInfo;
    property ParagraphFormattingOptions: TdxParagraphFormattingOptions read FParagraphFormattingOptions;
    property Tabs: TdxTabFormattingInfo read FTabs;
    property ListLevelIndex: Integer read FListLevelIndex;
    property NumberingListIndex: TdxNumberingListIndex read FNumberingListIndex;
    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;
    property FrameProperties: TdxFrameProperties read FFrameProperties;
  public
    constructor Create(AOutput: TdxMemoryStream); overload;
    constructor Create(AOutput: TdxMemoryStream; AParagraph: TdxParagraph); overload;
    constructor Create(AOutput: TdxMemoryStream; AProperties: TdxParagraphProperties; ATabs: TdxTabFormattingInfo); overload;
    constructor Create(AOutput: TdxMemoryStream; AListLevel: TdxListLevel); overload;
    constructor Create(AOutput: TdxMemoryStream; AParagraphStyleInfo: TdxDocParagraphStyleInfo); overload;
    destructor Destroy; override;
    procedure CreateTableParagraphPropertyModifiers(ATableDepth: Integer);
    procedure CreateParagarphPropertyModifiers;
    procedure AlignmentAction;
    procedure FirstLineIndentAction;
    procedure FirstLineIndentTypeAction;
    procedure LeftIndentAction;
    procedure LineSpacingAction;
    procedure LineSpacingTypeAction;
    procedure RightIndentAction;
    procedure SpacingAfterAction;
    procedure SpacingBeforeAction;
    procedure SuppressHyphenationAction;
    procedure SuppressLineNumbersAction;
    procedure ContextualSpacingAction;
    procedure PageBreakBeforeAction;
    procedure BeforeAutoSpacingAction;
    procedure AfterAutoSpacingAction;
    procedure KeepWithNextAction;
    procedure KeepLinesTogetherAction;
    procedure WidowOrphanControlAction;
    procedure OutlineLevelAction;
    procedure BackColorAction;
    procedure FramePropertiesAction;
    procedure LeftBorderAction;
    procedure RightBorderAction;
    procedure TopBorderAction;
    procedure BottomBorderAction;
    procedure DivIdAction;
    procedure InTableAction(ATableDepth: Integer);
    procedure TabsAction;
    procedure ListAction;
    procedure CalcLineSpacing; overload;
    function CalcLineSpacingTypeCode(AType: TdxParagraphLineSpacing): SmallInt;
    function CalcLineSpacing(AType: TdxParagraphLineSpacing): SmallInt; overload;
  end;

implementation

uses
  Math, Contnrs,
  dxEncoding,
  dxStringHelper,
  dxRichEdit.Import.Doc.DocCommand,
  dxRichEdit.Import.Doc.DCO;

{ TdxDocParagraphPropertiesActions }

constructor TdxDocParagraphPropertiesActions.Create(AOutput: TdxMemoryStream; AParagraphStyleInfo: TdxDocParagraphStyleInfo);
begin
  Create(AOutput);
  FParagraphFormattingInfo := AParagraphStyleInfo.ParagraphFormattingInfo;
  FParagraphFormattingOptions := AParagraphStyleInfo.ParagraphFormattingOptions;
  FTabs := AParagraphStyleInfo.TabFormattingInfo;
  FNumberingListIndex := AParagraphStyleInfo.NumberingListIndex;
  FListLevelIndex := AParagraphStyleInfo.ListLevelIndex;
  FDocumentModel := TdxDocumentModel(AParagraphStyleInfo.DocumentModel);
  FUnitConverter := FDocumentModel.UnitConverter;
end;

constructor TdxDocParagraphPropertiesActions.Create(AOutput: TdxMemoryStream; AListLevel: TdxListLevel);
begin
  Create(AOutput);
  FParagraphFormattingInfo := AListLevel.ParagraphProperties.Info.Info;
  FParagraphFormattingOptions := AListLevel.ParagraphProperties.Info.Options;
  FNumberingListIndex := -1;
  FDocumentModel := TdxDocumentModel(AListLevel.DocumentModel);
  FUnitConverter := FDocumentModel.UnitConverter;
  FTabs := AListLevel.Tabs.Info;
end;

constructor TdxDocParagraphPropertiesActions.Create(AOutput: TdxMemoryStream; AProperties: TdxParagraphProperties; ATabs: TdxTabFormattingInfo);
begin
  Create(AOutput);
  FParagraphFormattingInfo := AProperties.Info.Info;
  FParagraphFormattingOptions := AProperties.Info.Options;
  FTabs := ATabs;
  FNumberingListIndex := -1;
  FDocumentModel := TdxDocumentModel(AProperties.DocumentModel);
  FUnitConverter := FDocumentModel.UnitConverter;
end;

constructor TdxDocParagraphPropertiesActions.Create(AOutput: TdxMemoryStream; AParagraph: TdxParagraph);
begin
  Create(AOutput);
  FParagraphFormattingInfo := AParagraph.ParagraphProperties.Info.Info;
  FParagraphFormattingOptions := AParagraph.ParagraphProperties.Info.Options;
  FTabs := AParagraph.Tabs.Info;
  FNumberingListIndex := AParagraph.GetOwnNumberingListIndex;
  FListLevelIndex := AParagraph.GetOwnListLevelIndex;
  FDocumentModel := AParagraph.DocumentModel;
  FUnitConverter := AParagraph.DocumentModel.UnitConverter;
  FFrameProperties := AParagraph.FrameProperties;
end;

constructor TdxDocParagraphPropertiesActions.Create(AOutput: TdxMemoryStream);
begin
  Assert(AOutput <> nil, 'output');
  FWriter := TBinaryWriter.Create(AOutput);
end;

destructor TdxDocParagraphPropertiesActions.Destroy;
begin
  FWriter.Free;
  inherited Destroy;
end;

procedure TdxDocParagraphPropertiesActions.CreateTableParagraphPropertyModifiers(ATableDepth: Integer);
begin
  InTableAction(ATableDepth);
  CreateParagarphPropertyModifiers;
end;

procedure TdxDocParagraphPropertiesActions.CreateParagarphPropertyModifiers;
begin
  ListAction;
  TabsAction;
  AlignmentAction;
  FirstLineIndentAction;
  FirstLineIndentTypeAction;
  LeftIndentAction;
  LineSpacingAction;
  LineSpacingTypeAction;
  RightIndentAction;
  SpacingAfterAction;
  SpacingBeforeAction;
  SuppressHyphenationAction;
  SuppressLineNumbersAction;
  ContextualSpacingAction;
  PageBreakBeforeAction;
  BeforeAutoSpacingAction;
  AfterAutoSpacingAction;
  KeepWithNextAction;
  KeepLinesTogetherAction;
  WidowOrphanControlAction;
  OutlineLevelAction;
  BackColorAction;
  FramePropertiesAction;
  LeftBorderAction;
  RightBorderAction;
  TopBorderAction;
  BottomBorderAction;
  DivIdAction;
end;

procedure TdxDocParagraphPropertiesActions.AlignmentAction;
var
  ACommand: TdxDocCommandAlignmentNew;
begin
  if not ParagraphFormattingOptions.UseAlignment then
    Exit;
  ACommand := TdxDocCommandAlignmentNew.Create;
  try
    ACommand.Alignment := ParagraphFormattingInfo.Alignment;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.FirstLineIndentAction;
var
  AValue: Integer;
  ACommand: TdxDocCommandFirstLineIndent;
  ACommandNew: TdxDocCommandFirstLineIndentNew;
begin
  if not ParagraphFormattingOptions.UseFirstLineIndent then
    Exit;

  if (ParagraphFormattingInfo.FirstLineIndentType = TdxParagraphFirstLineIndent.Indented) then
    AValue := ParagraphFormattingInfo.FirstLineIndent
  else
    AValue := -ParagraphFormattingInfo.FirstLineIndent;
  ACommand := TdxDocCommandFirstLineIndent.Create;
  try
    ACommand.Value := AValue;
    ACommand.Write(FWriter);
    ACommandNew := TdxDocCommandFirstLineIndentNew.Create;
    try
      ACommandNew.Value := AValue;
      ACommandNew.Write(FWriter);
    finally
      ACommandNew.Free;
    end;
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.FirstLineIndentTypeAction;
begin
end;

procedure TdxDocParagraphPropertiesActions.LeftIndentAction;
var
  AValue: Integer;
  ALogicalCommand: TdxDocCommandLogicalLeftIndent;
  APhysicalCommand: TdxDocCommandPhysicalLeftIndent;
begin
  if not ParagraphFormattingOptions.UseLeftIndent then
    Exit;
  AValue := ParagraphFormattingInfo.LeftIndent;
  ALogicalCommand := TdxDocCommandLogicalLeftIndent.Create;
  try
    ALogicalCommand.Value := AValue;
    ALogicalCommand.Write(FWriter);
  finally
    ALogicalCommand.Free;
  end;
  APhysicalCommand := TdxDocCommandPhysicalLeftIndent.Create;
  try
    APhysicalCommand.Value := AValue;
    APhysicalCommand.Write(FWriter);
  finally
    APhysicalCommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.LineSpacingAction;
begin
  if not ParagraphFormattingOptions.UseLineSpacing then
    Exit;
  CalcLineSpacing;
end;

procedure TdxDocParagraphPropertiesActions.LineSpacingTypeAction;
begin
end;

procedure TdxDocParagraphPropertiesActions.RightIndentAction;
var
  AValue: Integer;
  ALogicalCommand: TdxDocCommandLogicalRightIndent;
  APhysicalCommand: TdxDocCommandPhysicalRightIndent;
begin
  if not ParagraphFormattingOptions.UseRightIndent then
    Exit;
  AValue := ParagraphFormattingInfo.RightIndent;
  ALogicalCommand := TdxDocCommandLogicalRightIndent.Create;
  try
    ALogicalCommand.Value := AValue;
    ALogicalCommand.Write(FWriter);
  finally
    ALogicalCommand.Free;
  end;
  APhysicalCommand := TdxDocCommandPhysicalRightIndent.Create;
  try
    APhysicalCommand.Value := AValue;
    APhysicalCommand.Write(FWriter);
  finally
    APhysicalCommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.SpacingAfterAction;
var
  ACommand: TdxDocCommandSpacingAfter;
begin
  if not ParagraphFormattingOptions.UseSpacingAfter then
    Exit;
  ACommand := TdxDocCommandSpacingAfter.Create;
  try
    ACommand.Value := ParagraphFormattingInfo.SpacingAfter;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.SpacingBeforeAction;
var
  ACommand: TdxDocCommandSpacingBefore;
begin
  if not ParagraphFormattingOptions.UseSpacingBefore then
    Exit;
  ACommand := TdxDocCommandSpacingBefore.Create;
  try
    ACommand.Value := ParagraphFormattingInfo.SpacingBefore;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.SuppressHyphenationAction;
var
  ACommand: TdxDocCommandSuppressHyphenation;
begin
  if not ParagraphFormattingOptions.UseSuppressHyphenation then
    Exit;
  ACommand := TdxDocCommandSuppressHyphenation.Create;
  try
    ACommand.Value := ParagraphFormattingInfo.SuppressHyphenation;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.SuppressLineNumbersAction;
var
  ACommand: TdxDocCommandSuppressLineNumbers;
begin
  if not ParagraphFormattingOptions.UseSuppressLineNumbers then
    Exit;
  ACommand := TdxDocCommandSuppressLineNumbers.Create;
  try
    ACommand.Value := ParagraphFormattingInfo.SuppressLineNumbers;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.ContextualSpacingAction;
var
  ACommand: TdxDocCommandContextualSpacing;
begin
  if not ParagraphFormattingOptions.UseContextualSpacing then
    Exit;
  ACommand := TdxDocCommandContextualSpacing.Create;
  try
    ACommand.Value := ParagraphFormattingInfo.ContextualSpacing;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.PageBreakBeforeAction;
var
  ACommand: TdxDocCommandPageBreakBefore;
begin
  if not ParagraphFormattingOptions.UsePageBreakBefore then
    Exit;
  ACommand := TdxDocCommandPageBreakBefore.Create;
  try
    ACommand.Value := ParagraphFormattingInfo.PageBreakBefore;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.BeforeAutoSpacingAction;
var
  ACommand: TdxDocCommandBeforeAutoSpacing;
begin
  if not ParagraphFormattingOptions.UseBeforeAutoSpacing then
    Exit;
  ACommand := TdxDocCommandBeforeAutoSpacing.Create;
  try
    ACommand.Value := ParagraphFormattingInfo.BeforeAutoSpacing;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.AfterAutoSpacingAction;
var
  ACommand: TdxDocCommandAfterAutoSpacing;
begin
  if not ParagraphFormattingOptions.UseAfterAutoSpacing then
    Exit;
  ACommand := TdxDocCommandAfterAutoSpacing.Create;
  try
    ACommand.Value := ParagraphFormattingInfo.AfterAutoSpacing;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.KeepWithNextAction;
var
  ACommand: TdxDocCommandKeepWithNext;
begin
  if not ParagraphFormattingOptions.UseKeepWithNext then
    Exit;
  ACommand := TdxDocCommandKeepWithNext.Create;
  try
    ACommand.Value := ParagraphFormattingInfo.KeepWithNext;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.KeepLinesTogetherAction;
var
  ACommand: TdxDocCommandKeepLinesTogether;
begin
  if not ParagraphFormattingOptions.UseKeepLinesTogether then
    Exit;
  ACommand := TdxDocCommandKeepLinesTogether.Create;
  try
    ACommand.Value := ParagraphFormattingInfo.KeepLinesTogether;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.WidowOrphanControlAction;
var
  ACommand: TdxDocCommandWidowOrphanControl;
begin
  if not ParagraphFormattingOptions.UseWidowOrphanControl then
    Exit;
  ACommand := TdxDocCommandWidowOrphanControl.Create;
  try
    ACommand.Value := ParagraphFormattingInfo.WidowOrphanControl;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.OutlineLevelAction;
var
  ACommand: TdxDocCommandOutlineLevel;
begin
  if not ParagraphFormattingOptions.UseOutlineLevel then
    Exit;
  ACommand := TdxDocCommandOutlineLevel.Create;
  try
    ACommand.Value := Byte(ParagraphFormattingInfo.OutlineLevel);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.BackColorAction;
var
  ACommand: TdxDocCommandParagraphShading;
begin
  if not ParagraphFormattingOptions.UseBackColor or (ParagraphFormattingInfo.BackColor = TdxAlphaColors.Transparent) then
    Exit;
  ACommand := TdxDocCommandParagraphShading.Create;
  try
    ACommand.ShadingDescriptor.BackgroundColor := ParagraphFormattingInfo.BackColor;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.FramePropertiesAction;
var
  AFrameWrapTypeCommand: TdxDocCommandFrameWrapType;
  AFrameHeightCommand: TdxDocCommandFrameHeight;
  AFrameWidthCommand: TdxDocCommandFrameWidth;
  AHorizontalPositionCommand: TdxDocCommandFrameHorizontalPosition;
  AVerticalPositionCommand: TdxDocCommandFrameVerticalPosition;
  AFramePositionCommand: TdxDocCommandFramePosition;
begin
  if FrameProperties = nil then
    Exit;
  AFrameWrapTypeCommand := TdxDocCommandFrameWrapType.Create;
  try
    AFrameWrapTypeCommand.WrapType := TdxDocWrapTypeCalculator.MapToDocWrapTypeStyle(FrameProperties.TextWrapType);
    AFrameWrapTypeCommand.Write(FWriter);
  finally
    AFrameWrapTypeCommand.Free;
  end;

  AFrameHeightCommand := TdxDocCommandFrameHeight.Create;
  try
    AFrameHeightCommand.Value := FrameProperties.Height;
    if FrameProperties.HorizontalRule = TdxParagraphFrameHorizontalRule.AtLeast then
      AFrameHeightCommand.MinHeight := True
    else
      AFrameHeightCommand.MinHeight := False;
    AFrameHeightCommand.Write(FWriter);
  finally
    AFrameHeightCommand.Free;
  end;

  AFrameWidthCommand := TdxDocCommandFrameWidth.Create;
  try
    AFrameWidthCommand.Value := FrameProperties.Width;
    AFrameWidthCommand.Write(FWriter);
  finally
    AFrameWidthCommand.Free;
  end;

  AHorizontalPositionCommand := TdxDocCommandFrameHorizontalPosition.Create;
  try
    AHorizontalPositionCommand.Value := UnitConverter.ModelUnitsToTwips(FrameProperties.X);
    AHorizontalPositionCommand.Write(FWriter);
  finally
    AHorizontalPositionCommand.Free;
  end;

  AVerticalPositionCommand := TdxDocCommandFrameVerticalPosition.Create;
  try
    AVerticalPositionCommand.Value := UnitConverter.ModelUnitsToTwips(FrameProperties.Y);
    AVerticalPositionCommand.Write(FWriter);
  finally
    AVerticalPositionCommand.Free;
  end;

  AFramePositionCommand := TdxDocCommandFramePosition.Create;
  try
    AFramePositionCommand.HorizontalAnchor := AFramePositionCommand.HorizontalTypeToDocHorizontalType(FrameProperties.HorizontalPositionType);
    AFramePositionCommand.VerticalAnchor := AFramePositionCommand.VerticalTypeToDocVerticalType(FrameProperties.VerticalPositionType);
    AFramePositionCommand.Write(FWriter);
  finally
    AFramePositionCommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.LeftBorderAction;
var
  ACommand: TdxDocCommandParagraphLeftBorderNew;
begin
  if not ParagraphFormattingOptions.UseLeftBorder then
    Exit;
  ACommand := TdxDocCommandParagraphLeftBorderNew.Create;
  try
    ACommand.CurrentBorder.ConvertFromBorderInfo(ParagraphFormattingInfo.LeftBorder, FUnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.RightBorderAction;
var
  ACommand: TdxDocCommandParagraphRightBorderNew;
begin
  if not ParagraphFormattingOptions.UseRightBorder then
    Exit;
  ACommand := TdxDocCommandParagraphRightBorderNew.Create;
  try
    ACommand.CurrentBorder.ConvertFromBorderInfo(ParagraphFormattingInfo.RightBorder, FUnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.TopBorderAction;
var
  ACommand: TdxDocCommandParagraphTopBorderNew;
begin
  if not ParagraphFormattingOptions.UseTopBorder then
    Exit;
  ACommand := TdxDocCommandParagraphTopBorderNew.Create;
  try
    ACommand.CurrentBorder.ConvertFromBorderInfo(ParagraphFormattingInfo.TopBorder, FUnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.BottomBorderAction;
var
  ACommand: TdxDocCommandParagraphBottomBorderNew;
begin
  if not ParagraphFormattingOptions.UseBottomBorder then
    Exit;
  ACommand := TdxDocCommandParagraphBottomBorderNew.Create;
  try
    ACommand.CurrentBorder.ConvertFromBorderInfo(ParagraphFormattingInfo.BottomBorder, FUnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.DivIdAction;
begin
end;

procedure TdxDocParagraphPropertiesActions.InTableAction(ATableDepth: Integer);
var
  AInTableCommand: TdxDocCommandInTable;
  ATableDepthCommand: TdxDocCommandTableDepth;
begin
  AInTableCommand := TdxDocCommandInTable.Create;
  try
    AInTableCommand.Value := True;
    AInTableCommand.Write(FWriter);
  finally
    AInTableCommand.Free;
  end;

  ATableDepthCommand := TdxDocCommandTableDepth.Create;
  try
    ATableDepthCommand.Value := ATableDepth;
    ATableDepthCommand.Write(FWriter);
  finally
    ATableDepthCommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.TabsAction;
var
  ACommand: TdxDocCommandChangeParagraphTabs;
begin
  if (Tabs = nil) or (Tabs.Count = 0) then
    Exit;
  ACommand := TdxDocCommandChangeParagraphTabs.Create;
  try
    ACommand.Tabs := Tabs;
    ACommand.UnitConverter := UnitConverter;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocParagraphPropertiesActions.ListAction;
var
  AListInfoCommand: TdxDocCommandListInfoIndex;
  ANumberingListIndex: Integer;
  ALevelCommand: TdxDocCommandListLevel;
begin
  if NumberingListIndex = NumberingListIndexNoNumberingList then
  begin
    AListInfoCommand := TdxDocCommandListInfoIndex.Create;
    try
      AListInfoCommand.Value := 0;
      AListInfoCommand.Write(FWriter);
    finally
      AListInfoCommand.Free;
    end;
  end
  else
  begin
    ANumberingListIndex := NumberingListIndex;
    if ANumberingListIndex < 0 then
      Exit;
    AListInfoCommand := TdxDocCommandListInfoIndex.Create;
    try
      AListInfoCommand.Value := ANumberingListIndex + 1;
      AListInfoCommand.Write(FWriter);
    finally
      AListInfoCommand.Free;
    end;

    ALevelCommand := TdxDocCommandListLevel.Create;
    try
      ALevelCommand.Value := Byte(ListLevelIndex);
      ALevelCommand.Write(FWriter);
    finally
      ALevelCommand.Free;
    end;
  end;
end;

procedure TdxDocParagraphPropertiesActions.CalcLineSpacing;
var
  ACommand: TdxDocCommandLineSpacing;
  AType: TdxParagraphLineSpacing;
begin
  ACommand := TdxDocCommandLineSpacing.Create;
  try
    AType := ParagraphFormattingInfo.LineSpacingType;
    ACommand.LineSpacingType := CalcLineSpacingTypeCode(AType);
    ACommand.LineSpacing := CalcLineSpacing(AType);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

function TdxDocParagraphPropertiesActions.CalcLineSpacingTypeCode(AType: TdxParagraphLineSpacing): SmallInt;
begin
  if (AType = TdxParagraphLineSpacing.AtLeast) or (AType = TdxParagraphLineSpacing.Exactly) then
    Result := 0
  else
    Result := 1;
end;

function TdxDocParagraphPropertiesActions.CalcLineSpacing(AType: TdxParagraphLineSpacing): SmallInt;
var
  ALineSpacing: Integer;
begin
  ALineSpacing := Trunc(ParagraphFormattingInfo.LineSpacing);
  case AType of
    TdxParagraphLineSpacing.Single:
      Result := 240;
    TdxParagraphLineSpacing.Sesquialteral:
      Result := 360;
    TdxParagraphLineSpacing.Double:
      Result := 480;
    TdxParagraphLineSpacing.Multiple:
      Result := Trunc(ParagraphFormattingInfo.LineSpacing * 240);
    TdxParagraphLineSpacing.Exactly:
      Result := Trunc(-1 * UnitConverter.ModelUnitsToTwips(ALineSpacing));
    else
      Result := Trunc(UnitConverter.ModelUnitsToTwips(ALineSpacing));
  end;
end;

end.
