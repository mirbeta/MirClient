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

unit dxRichEdit.Dialogs.EditStyleController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Graphics, Generics.Defaults, Generics.Collections, dxCoreGraphics,
  dxRichEdit.NativeApi,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.Utils.Types,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Dialogs.EditStyleHelper;

type

  { TdxEditStyleFormControllerParameters }

  TdxEditStyleFormControllerParameters = class(TdxFormControllerParameters)
  strict private
    FParagraphSourceStyle: TdxParagraphStyle;
    FTargetParagraphStyle: TdxParagraphStyle;
    FCharacterSourceStyle: TdxCharacterStyle;
    FTargetCharacterStyle: TdxCharacterStyle;
    FEditStyleControl: IdxRichEditControl;
    FParagraphIndex: TdxParagraphIndex;
    FIsParagraphStyle: Boolean;
    procedure SetParagraphSourceStyle(const AValue: TdxParagraphStyle);
    procedure SetCharacterSourceStyle(const AValue: TdxCharacterStyle);
  public
    constructor Create(const AControl: IdxRichEditControl; ASourceStyle: TdxParagraphStyle; AParagraphIndex: TdxParagraphIndex); overload;
    constructor Create(const AControl: IdxRichEditControl; ASourceStyle: TdxCharacterStyle; AParagraphIndex: TdxParagraphIndex); overload;
    destructor Destroy; override;

    property IsParagraphStyle: Boolean read FIsParagraphStyle;
    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex;
    property ParagraphSourceStyle: TdxParagraphStyle read FParagraphSourceStyle write SetParagraphSourceStyle;
    property CharacterSourceStyle: TdxCharacterStyle read FCharacterSourceStyle write SetCharacterSourceStyle;
    property TargetParagraphStyle: TdxParagraphStyle read FTargetParagraphStyle write FTargetParagraphStyle;
    property TargetCharacterStyle: TdxCharacterStyle read FTargetCharacterStyle write FTargetCharacterStyle;
    property EditStyleControl: IdxRichEditControl read FEditStyleControl;
  end;

  { TdxEditStyleFormController }

  TdxEditStyleFormController = class(TdxFormController)
  private const
    MaxTempParagraphSymbols = 256;
    EmptyParagraphRepeatCount = 21;
    FollowingParagraphRepeatCount = 25;
    PreviousParagraphRepeatCount = 10;
  strict private
    FParameters: TdxEditStyleFormControllerParameters;
    FParagraphIndex: TdxParagraphIndex;
    FIntermediateParagraphStyle: TdxParagraphStyle;
    FIntermediateCharacterStyle: TdxCharacterStyle;
    FControl: IdxRichEditControl;
    FModel: TdxDocumentModel;
    FIntermediateModel: TdxDocumentModel;
    function GetStyleName: string;
    procedure SetStyleName(const AValue: string);
    function GetIsParagraphStyle: Boolean;
    function GetParagraphSourceStyle: TdxParagraphStyle;
    function GetParagraphProperties: TdxParagraphProperties;
    function GetCharacterSourceStyle: TdxCharacterStyle;
    function GetCharacterProperties: TdxCharacterProperties;
    function GetUnitConverter: TdxDocumentModelUnitConverter;
  protected
    function FillParagraphText(ACount: Integer; const AId: Pointer): string;
    procedure SetActiveParagraph(ATarget: TdxPieceTable; ASourceParagraph: TdxParagraph);
    class procedure ApplyCharacterStyleToParagraph(AIndex: TdxParagraphIndex; ATarget: TdxPieceTable; AStyleIndex: Integer; ACStyleIndex: Integer); static;
    function GetAvailableStyleName(const ACurrentName: string): string;
    procedure ApplyStyleToParagraphs(const ARichEditControl: IdxRichEditControl);
  public
    constructor Create(const APreviewStyleControl: IdxRichEditControl; AParameters: TdxEditStyleFormControllerParameters);
    procedure ApplyChanges; override;
    procedure ApplyTabsProperties(const ARichEditControl: IdxRichEditControl);
    procedure CopyCharacterStyles(APreviewModel: TdxDocumentModel);
    procedure CopyParagraphStyles(APreviewModel: TdxDocumentModel);
    procedure CopyParagraphStyle(ATargetStyle: TdxParagraphStyle; AStyle: TdxParagraphStyle);
    procedure CopyCharacterStyle(ATargetStyle: TdxCharacterStyle; AStyle: TdxCharacterStyle);
    procedure CopyCharacterPropertiesFromMerged(AMergedProperties: TdxMergedCharacterProperties);
    procedure CopyParagraphPropertiesFromMerged(AMergedProperties: TdxMergedParagraphProperties);
    function GetParentParagraphProperties: TdxParagraphFormattingInfo;
    function GetParentCharacterProperties: TdxCharacterFormattingInfo;
    function GetCharacterPropertyAccessor: TdxCharacterPropertyAccessor;
    function GetParagraphPropertyAccessor: TdxParagraphPropertyAccessor;

    procedure ApplyParagraphStyle(AParagraph: TdxParagraph; AStyleIndex: Integer);
    procedure ApplyCharacterStyle(AParagraph: TdxParagraph; AStyleIndex: Integer);
    procedure ChangePreviewControlCurrentStyle(const ARichEditControl: IdxRichEditControl);
    function GetParagraphText(ASourceParagraph: TdxParagraph; AParagraphLength: Integer): string;
    procedure FillTempRichEdit(const ARichEditControl: IdxRichEditControl);
    function GetIntermediateMergedCharacterProperties(AStyle: TdxParagraphStyle): TdxCharacterFormattingInfo; overload;
    function GetIntermediateMergedCharacterProperties(AStyle: TdxCharacterStyle): TdxCharacterFormattingInfo; overload;
    function GetIntermediateMergedParagraphProperties(AStyle: TdxParagraphStyle): TdxParagraphFormattingInfo;
    function IsValidName(const AName: string): Boolean;
    procedure IncreaseSpacing;
    procedure IncreaseIndent;
    procedure DecreaseSpacing;
    procedure DecreaseIndent;

    property Control: IdxRichEditControl read FControl;
    property IntermediateModel: TdxDocumentModel read FIntermediateModel;
    property Model: TdxDocumentModel read FModel;
    property StyleName: string read GetStyleName write SetStyleName;
    property Parameters: TdxEditStyleFormControllerParameters read FParameters;
    property IsParagraphStyle: Boolean read GetIsParagraphStyle;
    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex;
    property ParagraphSourceStyle: TdxParagraphStyle read GetParagraphSourceStyle;
    property IntermediateParagraphStyle: TdxParagraphStyle read FIntermediateParagraphStyle write FIntermediateParagraphStyle;
    property CharacterSourceStyle: TdxCharacterStyle read GetCharacterSourceStyle;
    property IntermediateCharacterStyle: TdxCharacterStyle read FIntermediateCharacterStyle write FIntermediateCharacterStyle;
    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;

    property ParagraphProperties: TdxParagraphProperties read GetParagraphProperties;
    property CharacterProperties: TdxCharacterProperties read GetCharacterProperties;
  end;

implementation

uses
  StrUtils,
  dxCore,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Commands.Selection,
  dxRichEdit.DocumentModel.TabFormatting;

{ TdxEditStyleFormControllerParameters }

constructor TdxEditStyleFormControllerParameters.Create(const AControl: IdxRichEditControl;
  ASourceStyle: TdxParagraphStyle; AParagraphIndex: TdxParagraphIndex);
begin
  inherited Create(AControl);
  FParagraphSourceStyle := ASourceStyle;
  FTargetParagraphStyle := TdxParagraphStyle.Create(FParagraphSourceStyle.DocumentModel);
  FTargetCharacterStyle := TdxCharacterStyle.Create(FParagraphSourceStyle.DocumentModel, nil);
  FEditStyleControl := AControl;
  FParagraphIndex := AParagraphIndex;
  FIsParagraphStyle := True;
end;

constructor TdxEditStyleFormControllerParameters.Create(const AControl: IdxRichEditControl;
  ASourceStyle: TdxCharacterStyle; AParagraphIndex: TdxParagraphIndex);
begin
  inherited Create(AControl);
  FCharacterSourceStyle := ASourceStyle;
  FTargetCharacterStyle := TdxCharacterStyle.Create(FCharacterSourceStyle.DocumentModel, nil);
  FTargetParagraphStyle := TdxParagraphStyle.Create(FCharacterSourceStyle.DocumentModel);
  FEditStyleControl := AControl;
  FParagraphIndex := AParagraphIndex;
  FIsParagraphStyle := False;
end;

destructor TdxEditStyleFormControllerParameters.Destroy;
begin
  FTargetCharacterStyle.Free;
  FTargetParagraphStyle.Free;
  inherited Destroy;
end;

procedure TdxEditStyleFormControllerParameters.SetParagraphSourceStyle(const AValue: TdxParagraphStyle);
begin
  FParagraphSourceStyle := AValue;
  FIsParagraphStyle := True;
end;

procedure TdxEditStyleFormControllerParameters.SetCharacterSourceStyle(const AValue: TdxCharacterStyle);
begin
  FCharacterSourceStyle := AValue;
  FIsParagraphStyle := False;
end;

{ TdxEditStyleFormController }

constructor TdxEditStyleFormController.Create(const APreviewStyleControl: IdxRichEditControl; AParameters: TdxEditStyleFormControllerParameters);
begin
  inherited Create;
  FParameters := AParameters;
  FControl := AParameters.EditStyleControl;
  FParagraphIndex := AParameters.ParagraphIndex;
  FModel := AParameters.Control.InnerControl.DocumentModel;

  FIntermediateModel := APreviewStyleControl.DocumentModel;
  FIntermediateModel.BeginUpdate;
  try
    CopyCharacterStyles(FIntermediateModel);
    CopyParagraphStyles(FIntermediateModel);
  finally
    FIntermediateModel.EndUpdate;
  end;

  if IsParagraphStyle then
    FIntermediateParagraphStyle := FIntermediateModel.ParagraphStyles.GetStyleByName(ParagraphSourceStyle.StyleName) as TdxParagraphStyle
  else
    FIntermediateCharacterStyle := FIntermediateModel.CharacterStyles.GetStyleByName(CharacterSourceStyle.StyleName) as TdxCharacterStyle;

  FIntermediateModel.DefaultCharacterProperties.CopyFrom(FModel.DefaultCharacterProperties.Info);
  FIntermediateModel.DefaultParagraphProperties.CopyFrom(FModel.DefaultParagraphProperties.Info);
end;

function TdxEditStyleFormController.GetStyleName: string;
begin
  if IsParagraphStyle then
    Result := IntermediateParagraphStyle.LocalizedStyleName
  else
    Result := IntermediateCharacterStyle.LocalizedStyleName;
end;

procedure TdxEditStyleFormController.SetStyleName(const AValue: string);
begin
  if IsParagraphStyle then
    IntermediateParagraphStyle.StyleName := AValue
  else
    IntermediateCharacterStyle.StyleName := AValue;
end;

function TdxEditStyleFormController.GetIsParagraphStyle: Boolean;
begin
  Result := FParameters.IsParagraphStyle;
end;

function TdxEditStyleFormController.GetParagraphSourceStyle: TdxParagraphStyle;
begin
  Result := FParameters.ParagraphSourceStyle;
end;

function TdxEditStyleFormController.GetParagraphProperties: TdxParagraphProperties;
begin
  Result := IntermediateParagraphStyle.ParagraphProperties;
end;

function TdxEditStyleFormController.GetParagraphPropertyAccessor: TdxParagraphPropertyAccessor;
begin
  Result := TdxParagraphPropertyAccessor.Create(ParagraphProperties, GetParentParagraphProperties);
end;

function TdxEditStyleFormController.GetCharacterSourceStyle: TdxCharacterStyle;
begin
  Result := FParameters.CharacterSourceStyle;
end;

function TdxEditStyleFormController.GetCharacterProperties: TdxCharacterProperties;
begin
  if IsParagraphStyle then
    Result := IntermediateParagraphStyle.CharacterProperties
  else
    Result := IntermediateCharacterStyle.CharacterProperties;
end;

function TdxEditStyleFormController.GetCharacterPropertyAccessor: TdxCharacterPropertyAccessor;
begin
  Result := TdxCharacterPropertyAccessor.Create(CharacterProperties, GetParentCharacterProperties);
end;

function TdxEditStyleFormController.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := Control.DocumentModel.UnitConverter;
end;

procedure TdxEditStyleFormController.CopyCharacterStyles(APreviewModel: TdxDocumentModel);
var
  I: Integer;
  AStyle, ATargetStyle: TdxCharacterStyle;
begin
  for I := 0 to FModel.CharacterStyles.Count - 1 do
  begin
    AStyle := Model.CharacterStyles.Items[I];
    if AStyle.Deleted then
      Continue;

    ATargetStyle := TdxCharacterStyle(APreviewModel.CharacterStyles.GetStyleByName(AStyle.StyleName));
    if ATargetStyle <> nil then
      AStyle.CopyTo(ATargetStyle)
    else
      AStyle.Copy(APreviewModel);
  end;
end;

procedure TdxEditStyleFormController.CopyParagraphStyles(APreviewModel: TdxDocumentModel);
var
  I: Integer;
  AStyle, ATargetStyle: TdxParagraphStyle;
begin
  for I := 0 to FModel.ParagraphStyles.Count - 1 do
  begin
    AStyle := Model.ParagraphStyles.Items[I];
    if AStyle.Deleted then
      Continue;

    ATargetStyle := TdxParagraphStyle(APreviewModel.ParagraphStyles.GetStyleByName(AStyle.StyleName));
    if ATargetStyle <> nil then
      AStyle.CopyTo(ATargetStyle)
    else
      AStyle.Copy(APreviewModel);
  end;
end;

procedure TdxEditStyleFormController.CopyParagraphStyle(ATargetStyle: TdxParagraphStyle; AStyle: TdxParagraphStyle);
begin
  AStyle.CopyTo(ATargetStyle);
end;

procedure TdxEditStyleFormController.CopyCharacterStyle(ATargetStyle: TdxCharacterStyle; AStyle: TdxCharacterStyle);
begin
  AStyle.CopyTo(ATargetStyle);
end;

procedure TdxEditStyleFormController.ApplyChanges;
begin
  if IsParagraphStyle then
    CopyParagraphStyle(ParagraphSourceStyle, IntermediateParagraphStyle)
  else
    CopyCharacterStyle(CharacterSourceStyle, IntermediateCharacterStyle);
end;

function TdxEditStyleFormController.GetParentParagraphProperties: TdxParagraphFormattingInfo;
var
  AMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  if IntermediateParagraphStyle.Parent <> nil then
  begin
    AMergedParagraphProperties := IntermediateParagraphStyle.Parent.GetMergedWithDefaultParagraphProperties;
    try
      Result := AMergedParagraphProperties.Info.Clone;
    finally
      AMergedParagraphProperties.Free;
    end;
  end
  else
    Result := Control.InnerControl.DocumentModel.DefaultParagraphProperties.Info.Info.Clone;
end;

function TdxEditStyleFormController.GetParentCharacterProperties: TdxCharacterFormattingInfo;
var
  AMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  if IsParagraphStyle then
  begin
    if IntermediateParagraphStyle.Parent <> nil then
    begin
      AMergedCharacterProperties := IntermediateParagraphStyle.Parent.GetMergedWithDefaultCharacterProperties;
      try
        Exit(AMergedCharacterProperties.Info.Clone);
      finally
        AMergedCharacterProperties.Free;
      end;
    end;
  end
  else
  begin
    if IntermediateCharacterStyle.Parent <> nil then
    begin
      AMergedCharacterProperties := IntermediateCharacterStyle.Parent.GetMergedWithDefaultCharacterProperties;
      try
        Exit(AMergedCharacterProperties.Info.Clone);
      finally
        AMergedCharacterProperties.Free;
      end;
    end;
  end;
  Result := Control.InnerControl.DocumentModel.DefaultCharacterProperties.Info.Info.Clone;
end;

function TdxEditStyleFormController.FillParagraphText(ACount: Integer; const AId: Pointer): string;
begin
  Result := DupeString(cxGetResourceString(AId), ACount);
end;

function TdxEditStyleFormController.GetParagraphText(ASourceParagraph: TdxParagraph; AParagraphLength: Integer): string;
var
  ASource: TdxPieceTable;
  AStartPosition, AEndPosition: TdxDocumentModelPosition;
begin
  ASource := FControl.InnerControl.DocumentModel.ActivePieceTable;
  AStartPosition := TdxPositionConverter.ToDocumentModelPosition(ASource, ASourceParagraph.LogPosition);
  AEndPosition := TdxPositionConverter.ToDocumentModelPosition(ASource, ASourceParagraph.LogPosition + AParagraphLength - 1);
  Result := ASource.GetPlainText(AStartPosition, AEndPosition);
end;

procedure TdxEditStyleFormController.SetActiveParagraph(ATarget: TdxPieceTable; ASourceParagraph: TdxParagraph);
var
  AParagraphLength: Integer;
  AParagraphText, AActiveParagraphText: string;
begin
  if ASourceParagraph.LogPosition <> ASourceParagraph.EndLogPosition then
  begin
    if ASourceParagraph.Length < MaxTempParagraphSymbols then
      AParagraphLength := ASourceParagraph.Length
    else
      AParagraphLength := MaxTempParagraphSymbols;
    AParagraphText := GetParagraphText(ASourceParagraph, AParagraphLength);
    if AParagraphText <> '' then
    begin
      ATarget.InsertPlainText(0, AParagraphText);
      Exit;
    end;
  end;
  AActiveParagraphText := FillParagraphText(EmptyParagraphRepeatCount, @sdxRichEditEditStyleDialogCurrentParagraphText);
  ATarget.InsertPlainText(0, AActiveParagraphText);
end;

class procedure TdxEditStyleFormController.ApplyCharacterStyleToParagraph(AIndex: TdxParagraphIndex; ATarget: TdxPieceTable; AStyleIndex: Integer; ACStyleIndex: Integer);
var
  AStart, AEnd, I: TdxRunIndex;
begin
  ATarget.Paragraphs[AIndex].ParagraphStyleIndex := AStyleIndex;
  AStart := ATarget.Paragraphs[AIndex].FirstRunIndex;
  AEnd := ATarget.Paragraphs[AIndex].LastRunIndex;
  for I := AStart to AEnd - 1 do
    ATarget.Runs[I].CharacterStyleIndex := ACStyleIndex;
end;

function TdxEditStyleFormController.GetAvailableStyleName(const ACurrentName: string): string;
var
  AStylesCount, I: Integer;
begin
  if IsValidName(ACurrentName) then
    Exit(ACurrentName);
  AStylesCount := Model.CharacterStyles.Count + Model.ParagraphStyles.Count;
  for I := 1 to AStylesCount do
    if IsValidName(ACurrentName + IntToStr(I)) then
      Exit(ACurrentName + IntToStr(I));
  Result := Format('%s%d1', [ACurrentName, AStylesCount]);
end;

procedure TdxEditStyleFormController.ApplyStyleToParagraphs(const ARichEditControl: IdxRichEditControl);
var
  AModel: TdxDocumentModel;
  ATarget: TdxPieceTable;
  ATempParagraphStyle: TdxParagraphStyle;
  AStyleIndex, ACStyleIndex: Integer;
  ADefaultCharacterStyle: TdxCharacterStyle;
begin
  AModel := ARichEditControl.InnerControl.DocumentModel;
  ATarget := AModel.ActivePieceTable;

  ATempParagraphStyle := TdxParagraphStyle.Create(AModel);
  try
    ATempParagraphStyle.StyleName := GetAvailableStyleName('_TempParagraphStyle');
    ATempParagraphStyle.CharacterProperties.FontName := 'Times New Roman';
    ATempParagraphStyle.CharacterProperties.DoubleFontSize := 20;
    ATempParagraphStyle.CharacterProperties.ForeColor := TdxAlphaColors.FromArgb(191, 191, 191);
    if IsParagraphStyle then
      ATempParagraphStyle.Tabs.CopyFrom(IntermediateParagraphStyle.Tabs);

    AStyleIndex := ATempParagraphStyle.Copy(AModel);

    ADefaultCharacterStyle := TdxCharacterStyle.Create(AModel, nil);
    try
      ADefaultCharacterStyle.StyleName := GetAvailableStyleName('_DefaultCharacterStyle');
      ACStyleIndex := ADefaultCharacterStyle.Copy(AModel);
      ApplyCharacterStyleToParagraph(0, ATarget, AStyleIndex, ACStyleIndex);
      ApplyCharacterStyleToParagraph(2, ATarget, AStyleIndex, ACStyleIndex);
    finally
      ADefaultCharacterStyle.Free;
    end;
  finally
    ATempParagraphStyle.Free;
  end;
end;

procedure TdxEditStyleFormController.ApplyTabsProperties(const ARichEditControl: IdxRichEditControl);
var
  ATableModel: TdxDocumentModel;
  AParagraph: TdxParagraph;
begin
  ATableModel := ARichEditControl.DocumentModel;
  AParagraph := ATableModel.ActivePieceTable.Paragraphs[0];
  IntermediateParagraphStyle.Tabs.CopyFrom(AParagraph.Tabs);
end;

procedure TdxEditStyleFormController.ChangePreviewControlCurrentStyle(const ARichEditControl: IdxRichEditControl);
var
  ATableModel: TdxDocumentModel;
  AParagraph: TdxParagraph;
begin
  ATableModel := ARichEditControl.DocumentModel;
  AParagraph := ATableModel.ActivePieceTable.Paragraphs[1];

  ATableModel.BeginUpdate;
  try
    if IsParagraphStyle then
    begin
      ApplyParagraphStyle(AParagraph, ATableModel.ParagraphStyles.GetStyleIndexByName(IntermediateParagraphStyle.StyleName));
      ApplyCharacterStyle(AParagraph, ATableModel.CharacterStyles.Count - 1);
    end
    else
    begin
      ApplyParagraphStyle(AParagraph, ATableModel.ParagraphStyles.Count - 1);
      ApplyCharacterStyle(AParagraph, ATableModel.CharacterStyles.GetStyleIndexByName(IntermediateCharacterStyle.StyleName));
    end;
  finally
    ATableModel.EndUpdate;
  end;
end;

procedure TdxEditStyleFormController.ApplyParagraphStyle(AParagraph: TdxParagraph; AStyleIndex: Integer);
begin
  AParagraph.ParagraphStyleIndex := AStyleIndex;
end;

procedure TdxEditStyleFormController.ApplyCharacterStyle(AParagraph: TdxParagraph; AStyleIndex: Integer);
var
  APieceTable: TdxPieceTable;
begin
  APieceTable := AParagraph.PieceTable;
  if AParagraph.Length > 0 then
    APieceTable.ApplyCharacterStyle(AParagraph.LogPosition, AParagraph.Length, AStyleIndex);
end;

procedure TdxEditStyleFormController.FillTempRichEdit(const ARichEditControl: IdxRichEditControl);
var
  ASourceModel, ATableModel: TdxDocumentModel;
  ASourcePieceTable, ATargetPieceTable: TdxPieceTable;
  ASourceParagraph: TdxParagraph;
  ASecondParagraphText, AFirstParagraphText: string;
  ADefaultParagraphStyle: TdxParagraphStyle;
  AStartOfDocumentCommand: TdxStartOfDocumentCommand;
begin
  ASourceModel := Control.InnerControl.DocumentModel;
  ASourcePieceTable := ASourceModel.ActivePieceTable;
  ATableModel := ARichEditControl.DocumentModel;
  ATargetPieceTable := ATableModel.ActivePieceTable;

  ASourceParagraph := ASourcePieceTable.Paragraphs[ParagraphIndex];

  ASecondParagraphText := #13 + FillParagraphText(FollowingParagraphRepeatCount, @sdxRichEditEditStyleDialogFollowingParagraphText);
  ATargetPieceTable.InsertPlainText(0, ASecondParagraphText);

  SetActiveParagraph(ATargetPieceTable, ASourceParagraph);

  AFirstParagraphText := FillParagraphText(PreviousParagraphRepeatCount, @sdxRichEditEditStyleDialogPreviousParagraphText) + #13;
  ATargetPieceTable.InsertPlainText(0, AFirstParagraphText);

  ApplyStyleToParagraphs(ARichEditControl);

  ADefaultParagraphStyle := TdxParagraphStyle.Create(ATableModel);
  try
    ADefaultParagraphStyle.StyleName := GetAvailableStyleName('_DefaultParagraphStyle');
    ADefaultParagraphStyle.Copy(ATableModel);
  finally
    ADefaultParagraphStyle.Free;
  end;

  AStartOfDocumentCommand := TdxStartOfDocumentCommand.Create(ARichEditControl);
  try
    AStartOfDocumentCommand.Execute;
  finally
    AStartOfDocumentCommand.Free;
  end;
end;

procedure TdxEditStyleFormController.CopyCharacterPropertiesFromMerged(AMergedProperties: TdxMergedCharacterProperties);
var
  AMergedCharacterProperties, ACharacterProperties: TdxCharacterFormattingInfo;
  AAccessor: TdxCharacterPropertyAccessor;
begin
  AMergedCharacterProperties := AMergedProperties.Info;
  if IsParagraphStyle then
    ACharacterProperties := GetIntermediateMergedCharacterProperties(IntermediateParagraphStyle)
  else
    ACharacterProperties := GetIntermediateMergedCharacterProperties(IntermediateCharacterStyle);
  try
    AAccessor := GetCharacterPropertyAccessor;
    try
      if ACharacterProperties.FontBold <> AMergedCharacterProperties.FontBold then
        AAccessor.ChangeFontBold(AMergedCharacterProperties.FontBold);
      if ACharacterProperties.FontItalic <> AMergedCharacterProperties.FontItalic then
        AAccessor.ChangeFontItalic(AMergedCharacterProperties.FontItalic);
      if ACharacterProperties.DoubleFontSize <> AMergedCharacterProperties.DoubleFontSize then
        AAccessor.ChangeDoubleFontSize(AMergedCharacterProperties.DoubleFontSize);
      if ACharacterProperties.ForeColor <> AMergedCharacterProperties.ForeColor then
        AAccessor.ChangeForeColor(AMergedCharacterProperties.ForeColor);
      if CharacterProperties.FontUnderlineType <> AMergedCharacterProperties.FontUnderlineType then
        AAccessor.ChangeFontUnderlineType(AMergedCharacterProperties.FontUnderlineType);
      if ACharacterProperties.UnderlineColor <> AMergedCharacterProperties.UnderlineColor then
        AAccessor.ChangeUnderlineColor(AMergedCharacterProperties.UnderlineColor);
      if ACharacterProperties.FontStrikeoutType <> AMergedCharacterProperties.FontStrikeoutType then
        AAccessor.ChangeFontStrikeoutType(AMergedCharacterProperties.FontStrikeoutType);
      if ACharacterProperties.UnderlineWordsOnly <> AMergedCharacterProperties.UnderlineWordsOnly then
        AAccessor.ChangeUnderlineWordsOnly(AMergedCharacterProperties.UnderlineWordsOnly);
      if ACharacterProperties.Script <> AMergedCharacterProperties.Script then
        AAccessor.ChangeScript(AMergedCharacterProperties.Script);
      if ACharacterProperties.AllCaps <> AMergedCharacterProperties.AllCaps then
        AAccessor.ChangeAllCaps(AMergedCharacterProperties.AllCaps);
      if ACharacterProperties.Hidden <> AMergedCharacterProperties.Hidden then
        AAccessor.ChangeHidden(AMergedCharacterProperties.Hidden);
    finally
      AAccessor.Free;
    end;
  finally
    ACharacterProperties.Free
  end;
end;

procedure TdxEditStyleFormController.CopyParagraphPropertiesFromMerged(AMergedProperties: TdxMergedParagraphProperties);
var
  AMergedParagraphProperties, AParagraphProperties: TdxParagraphFormattingInfo;
  AAccessor: TdxParagraphPropertyAccessor;
begin
  AMergedParagraphProperties := AMergedProperties.Info;
  AParagraphProperties := GetIntermediateMergedParagraphProperties(IntermediateParagraphStyle);
  try
    AAccessor := GetParagraphPropertyAccessor;
    try
      if AParagraphProperties.Alignment <> AMergedParagraphProperties.Alignment then
        AAccessor.ChangeAlignment(AMergedParagraphProperties.Alignment);
      if AParagraphProperties.OutlineLevel <> AMergedParagraphProperties.OutlineLevel then
        AAccessor.ChangeOutlineLevel(AMergedParagraphProperties.OutlineLevel);
      if AParagraphProperties.LeftIndent <> AMergedParagraphProperties.LeftIndent then
        AAccessor.ChangeLeftIndent(AMergedParagraphProperties.LeftIndent);
      if AParagraphProperties.RightIndent <> AMergedParagraphProperties.RightIndent then
        AAccessor.ChangeRightIndent(AMergedParagraphProperties.RightIndent);
      if AParagraphProperties.FirstLineIndentType <> AMergedParagraphProperties.FirstLineIndentType then
        ParagraphProperties.FirstLineIndentType := AMergedParagraphProperties.FirstLineIndentType;
      if AParagraphProperties.FirstLineIndent <> AMergedParagraphProperties.FirstLineIndent then
        AAccessor.ChangeFirstLineIndent(AMergedParagraphProperties.FirstLineIndent);
      if AParagraphProperties.SpacingBefore <> AMergedParagraphProperties.SpacingBefore then
        AAccessor.ChangeSpacingBefore(AMergedParagraphProperties.SpacingBefore);
      if AParagraphProperties.SpacingAfter <> AMergedParagraphProperties.SpacingAfter then
        AAccessor.ChangeSpacingAfter(AMergedParagraphProperties.SpacingAfter);
      if AParagraphProperties.LineSpacingType <> AMergedParagraphProperties.LineSpacingType then
        ParagraphProperties.LineSpacingType := AMergedParagraphProperties.LineSpacingType;
      if AParagraphProperties.LineSpacing <> AMergedParagraphProperties.LineSpacing then
        AAccessor.ChangeLineSpacing(AMergedParagraphProperties.LineSpacing);
      if AParagraphProperties.BeforeAutoSpacing <> AMergedParagraphProperties.BeforeAutoSpacing then
        AAccessor.ChangeBeforeAutoSpacing(AMergedParagraphProperties.BeforeAutoSpacing);
      if AParagraphProperties.AfterAutoSpacing <> AMergedParagraphProperties.AfterAutoSpacing then
        AAccessor.ChangeAfterAutoSpacing(AMergedParagraphProperties.AfterAutoSpacing);
      if AParagraphProperties.ContextualSpacing <> AMergedParagraphProperties.ContextualSpacing then
        AAccessor.ChangeContextualSpacing(AMergedParagraphProperties.ContextualSpacing);
      if AParagraphProperties.KeepLinesTogether <> AMergedParagraphProperties.KeepLinesTogether then
        AAccessor.ChangeKeepLinesTogether(AMergedParagraphProperties.KeepLinesTogether);
      if AParagraphProperties.KeepWithNext <> AMergedParagraphProperties.KeepWithNext then
        AAccessor.ChangeKeepWithNext(AMergedParagraphProperties.KeepWithNext);
      if AParagraphProperties.PageBreakBefore <> AMergedParagraphProperties.PageBreakBefore then
        AAccessor.ChangePageBreakBefore(AMergedParagraphProperties.PageBreakBefore);
      if AParagraphProperties.SuppressLineNumbers <> AMergedParagraphProperties.SuppressLineNumbers then
        AAccessor.ChangeSuppressLineNumbers(AMergedParagraphProperties.SuppressLineNumbers);
      if AParagraphProperties.SuppressHyphenation <> AMergedParagraphProperties.SuppressHyphenation then
        AAccessor.ChangeSuppressHyphenation(AMergedParagraphProperties.SuppressHyphenation);
      if AParagraphProperties.WidowOrphanControl <> AMergedParagraphProperties.WidowOrphanControl then
        AAccessor.ChangeWidowOrphanControl(AMergedParagraphProperties.WidowOrphanControl);
      if AParagraphProperties.BackColor <> AMergedParagraphProperties.BackColor then
        AAccessor.ChangeBackColor(AMergedParagraphProperties.BackColor);
    finally
      AAccessor.Free;
    end;
  finally
    AParagraphProperties.Free;
  end;
end;

procedure TdxEditStyleFormController.IncreaseSpacing;
var
  ANewValue: Integer;
begin
  ANewValue := UnitConverter.PointsToModelUnits(6);
  ParagraphProperties.SpacingBefore := ParagraphProperties.SpacingBefore + ANewValue;
  ParagraphProperties.SpacingAfter := ParagraphProperties.SpacingAfter + ANewValue;
end;

procedure TdxEditStyleFormController.DecreaseSpacing;
var
  ANewValue: Integer;
begin
  ANewValue := UnitConverter.PointsToModelUnits(6);
  if ParagraphProperties.SpacingBefore < ANewValue then
    ParagraphProperties.SpacingBefore := 0
  else
    ParagraphProperties.SpacingBefore := ParagraphProperties.SpacingBefore - ANewValue;

  if ParagraphProperties.SpacingAfter < ANewValue then
    ParagraphProperties.SpacingAfter := 0
  else
    ParagraphProperties.SpacingAfter := ParagraphProperties.SpacingAfter - ANewValue;
end;

procedure TdxEditStyleFormController.DecreaseIndent;
var
  ANewValue: Integer;
begin
  ANewValue := Trunc(UnitConverter.CentimetersToModelUnitsF(1.27));
  if ParagraphProperties.LeftIndent < ANewValue then
  begin
    ParagraphProperties.LeftIndent := 0;
  end
  else
    ParagraphProperties.LeftIndent := ParagraphProperties.LeftIndent - ANewValue;
end;

procedure TdxEditStyleFormController.IncreaseIndent;
var
  ANewValue: Integer;
begin
  ANewValue := Trunc(UnitConverter.CentimetersToModelUnitsF(1.27));
  ParagraphProperties.LeftIndent := ParagraphProperties.LeftIndent + ANewValue;
end;

function TdxEditStyleFormController.GetIntermediateMergedCharacterProperties(AStyle: TdxParagraphStyle): TdxCharacterFormattingInfo;
var
  AMergedProperties: TdxMergedCharacterProperties;
begin
  AMergedProperties := AStyle.GetMergedCharacterProperties;
  try
    AMergedProperties.Merge((AStyle.DocumentModel as TdxDocumentModel).DefaultCharacterProperties);
    Result := AMergedProperties.Info.Clone;
  finally
    AMergedProperties.Free;
  end;
end;

function TdxEditStyleFormController.GetIntermediateMergedCharacterProperties(AStyle: TdxCharacterStyle): TdxCharacterFormattingInfo;
var
  AMergedProperties: TdxMergedCharacterProperties;
begin
  AMergedProperties := AStyle.GetMergedCharacterProperties;
  try
    AMergedProperties.Merge((AStyle.DocumentModel as TdxDocumentModel).DefaultCharacterProperties);
    Result := AMergedProperties.Info.Clone;
  finally
    AMergedProperties.Free;
  end;
end;

function TdxEditStyleFormController.GetIntermediateMergedParagraphProperties(AStyle: TdxParagraphStyle): TdxParagraphFormattingInfo;
var
  AMergedProperties: TdxMergedParagraphProperties;
begin
  AMergedProperties := AStyle.GetMergedParagraphProperties;
  try
    AMergedProperties.Merge((AStyle.DocumentModel as TdxDocumentModel).DefaultParagraphProperties);
    Result := AMergedProperties.Info.Clone;
  finally
    AMergedProperties.Free;
  end;
end;

function TdxEditStyleFormController.IsValidName(const AName: string): Boolean;
var
  AModel: TdxDocumentModel;
begin
  AModel := Control.InnerControl.DocumentModel;
  if IsParagraphStyle then
    Result := TdxEditStyleHelper.IsValidStyleName(AName, ParagraphSourceStyle, AModel.ParagraphStyles)
  else
    Result := TdxEditStyleHelper.IsValidStyleName(AName, CharacterSourceStyle, AModel.CharacterStyles);
end;

end.

