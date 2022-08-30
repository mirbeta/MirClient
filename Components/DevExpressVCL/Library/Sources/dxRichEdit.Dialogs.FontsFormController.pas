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

unit dxRichEdit.Dialogs.FontsFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Graphics, dxCoreGraphics, dxCoreClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxRichEdit.Commands.Numbering,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.View.Core,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.CustomDialog;

type
   TdxNullableCharacterFormattingScript = TdxNullableValue<TdxCharacterFormattingScript>;
   TdxNullableStrikeoutType = TdxNullableValue<TdxStrikeoutType>;
   TdxNullableUnderlineType = TdxNullableValue<TdxUnderlineType>;
   TdxNullableColor = TdxNullableValue<TdxAlphaColor>;

  { TdxFontFormControllerParameters }

  TdxFontFormControllerParameters = class(TdxFormControllerParameters)
  private
    FSourceCharacterProperties: TdxMergedCharacterProperties;
  public
    constructor Create(const AControl: IdxRichEditControl; ASourceCharacterProperties: TdxMergedCharacterProperties);
    property SourceCharacterProperties: TdxMergedCharacterProperties read FSourceCharacterProperties;
  end;

  TdxFontFormController = class(TdxFormController)
  private
    FSourceCharacterProperties: TdxMergedCharacterProperties;
    FAllCaps: TdxNullableBoolean;
    FScript: TdxNullableCharacterFormattingScript;
    FFontStrikeoutType: TdxNullableStrikeoutType;
    FFontName: string;
    FFontBold: TdxNullableBoolean;
    FFontItalic: TdxNullableBoolean;
    FFontSize: TdxNullableInteger;
    FDoubleFontSize: TdxNullableInteger;
    FForeColor: TdxNullableColor;
    FUnderlineColor: TdxNullableColor;
    FFontUnderlineType: TdxNullableUnderlineType;
    FPrevFontUnderlineType: TdxNullableUnderlineType;
    FUnderlineWordsOnly: TdxNullableBoolean;
    FInitUnderlineWordsOnly: Boolean;
    FHidden: TdxNullableBoolean;
    procedure SetFontUnderlineType(const Value: TdxNullableUnderlineType);
    procedure SetUnderlineWordsOnly(const Value: TdxNullableBoolean);
    function GetSourceOptions: PdxCharacterFormattingOptions; inline;
    function GetSourceInfo: TdxCharacterFormattingInfo; inline;
  protected
    procedure ApplyAllCaps;
    procedure ApplyDoubleFontSize;
    procedure ApplyFontBold;
    procedure ApplyForeColor;
    procedure ApplyFontItalic;
    procedure ApplyFontName;
    procedure ApplyFontUnderlineType;
    procedure ApplyFontStrikeoutType;
    procedure ApplyScript;
    procedure ApplyHidden;
    procedure ApplyUnderlineColor;
    procedure ApplyUnderlineWordsOnly;
    procedure InitializeController;
    property SourceOptions: PdxCharacterFormattingOptions read GetSourceOptions;
    property SourceInfo: TdxCharacterFormattingInfo read GetSourceInfo;
  public
    constructor Create(const AControllerParameters: TdxFontFormControllerParameters);
    procedure ApplyChanges; override;
    procedure SetFontUnderline(const AUnderlineType: TdxNullableUnderlineType; const AUnderlineWordsOnly: TdxNullableBoolean);

    property AllCaps: TdxNullableBoolean read FAllCaps write FAllCaps;
    property Script: TdxNullableCharacterFormattingScript read FScript write FScript;
    property FontStrikeoutType: TdxNullableStrikeoutType read FFontStrikeoutType write FFontStrikeoutType;
    property FontName: string read FFontName write FFontName;
    property FontBold: TdxNullableBoolean read FFontBold write FFontBold;
    property FontItalic: TdxNullableBoolean read FFontItalic write FFontItalic;
    property FontSize: TdxNullableInteger read FFontSize write FFontSize;
    property DoubleFontSize: TdxNullableInteger read FDoubleFontSize write FDoubleFontSize;
    property ForeColor: TdxNullableColor read FForeColor write FForeColor;
    property UnderlineColor: TdxNullableColor read FUnderlineColor write FUnderlineColor;
    property FontUnderlineType: TdxNullableUnderlineType read FFontUnderlineType write SetFontUnderlineType;
    property UnderlineWordsOnly: TdxNullableBoolean read FUnderlineWordsOnly write SetUnderlineWordsOnly;
    property Hidden: TdxNullableBoolean read FHidden write FHidden;

    property SourceCharacterProperties: TdxMergedCharacterProperties read FSourceCharacterProperties;
  end;

implementation

uses
  Math, StrUtils;

{ TdxFontFormControllerParameters }

constructor TdxFontFormControllerParameters.Create(const AControl: IdxRichEditControl; ASourceCharacterProperties: TdxMergedCharacterProperties);
begin
  inherited Create(AControl);
  Assert(Assigned(ASourceCharacterProperties));
  FSourceCharacterProperties := ASourceCharacterProperties;
end;

{ TdxFontFormController }

function TdxFontFormController.GetSourceInfo: TdxCharacterFormattingInfo;
begin
  Result := SourceCharacterProperties.Info;
end;

function TdxFontFormController.GetSourceOptions: PdxCharacterFormattingOptions;
begin
  Result := @SourceCharacterProperties.Options;
end;

procedure TdxFontFormController.ApplyAllCaps;
begin
  SourceOptions.UseAllCaps := not AllCaps.IsNull and ((SourceInfo.AllCaps <> AllCaps.Value) or not SourceOptions.UseAllCaps);
  if SourceOptions.UseAllCaps then
    SourceInfo.AllCaps := AllCaps.Value;
end;

procedure TdxFontFormController.ApplyChanges;
begin
  ApplyAllCaps;
  ApplyFontName;
  ApplyFontBold;
  ApplyFontItalic;
  ApplyDoubleFontSize;
  ApplyForeColor;
  ApplyFontUnderlineType;
  ApplyUnderlineColor;
  ApplyFontStrikeoutType;
  ApplyScript;
  ApplyUnderlineWordsOnly;
  ApplyHidden;
end;

procedure TdxFontFormController.ApplyDoubleFontSize;
begin
  SourceOptions.UseDoubleFontSize := not DoubleFontSize.IsNull and ((SourceInfo.DoubleFontSize <> DoubleFontSize.Value) or not SourceOptions.UseDoubleFontSize);
  if SourceOptions.UseDoubleFontSize then
    SourceInfo.DoubleFontSize := Max(1, DoubleFontSize.Value);
end;

procedure TdxFontFormController.ApplyFontBold;
begin
  SourceOptions.UseFontBold := not FontBold.IsNull and ((SourceInfo.FontBold <> FontBold.Value) or not SourceOptions.UseFontBold);
  if SourceOptions.UseFontBold then
    SourceInfo.FontBold := FontBold.Value;
end;

procedure TdxFontFormController.ApplyFontItalic;
begin
  SourceOptions.UseFontItalic := not FontItalic.IsNull and ((SourceInfo.FontItalic <> FontItalic.Value) or not SourceOptions.UseFontItalic);
  if SourceOptions.UseFontItalic then
    SourceInfo.FontItalic := FontItalic.Value;
end;

procedure TdxFontFormController.ApplyFontName;
begin
  SourceOptions.UseFontName := (FontName <> '') and ((SourceInfo.FontName <> FontName) or not SourceOptions.UseFontName);
  if SourceOptions.UseFontName then
    SourceInfo.FontName := FontName;
end;

procedure TdxFontFormController.ApplyFontStrikeoutType;
begin
  SourceOptions.UseFontStrikeoutType := not FontStrikeoutType.IsNull and
    ((SourceInfo.FontStrikeoutType <> FontStrikeoutType.Value) or not SourceOptions.UseFontStrikeoutType);
  if SourceOptions.UseFontStrikeoutType then
    SourceInfo.FontStrikeoutType := FontStrikeoutType.Value;
end;

procedure TdxFontFormController.ApplyFontUnderlineType;
begin
  SourceOptions.UseFontUnderlineType := not FontUnderlineType.IsNull and
    (not SourceOptions.UseFontUnderlineType or (SourceInfo.FontUnderlineType <> FontUnderlineType.Value));
  if SourceOptions.UseFontUnderlineType then
    SourceInfo.FontUnderlineType := FontUnderlineType.Value;
end;

procedure TdxFontFormController.ApplyForeColor;
begin
  SourceOptions.UseForeColor := not ForeColor.IsNull and
    ((SourceInfo.ForeColor <> ForeColor.Value) or not SourceOptions.UseForeColor);
  if SourceOptions.UseForeColor then
    SourceInfo.ForeColor := ForeColor.Value;
end;

procedure TdxFontFormController.ApplyHidden;
begin
  SourceOptions.UseHidden := not Hidden.IsNull and ((SourceInfo.Hidden <> Hidden.Value) or not SourceOptions.UseHidden);
  if SourceOptions.UseHidden then
    SourceInfo.Hidden := Hidden.Value;
end;

procedure TdxFontFormController.ApplyScript;
begin
  SourceOptions.UseScript := not Script.IsNull and ((SourceInfo.Script <> Script.Value) or not SourceOptions.UseScript);
  if SourceOptions.UseScript then
    SourceInfo.Script := Script.Value;
end;

procedure TdxFontFormController.ApplyUnderlineColor;
begin
  SourceOptions.UseUnderlineColor := not UnderlineColor.IsNull and
    ((SourceInfo.UnderlineColor <> UnderlineColor.Value) or not SourceOptions.UseUnderlineColor);
  if SourceOptions.UseUnderlineColor then
    SourceInfo.UnderlineColor := UnderlineColor.Value;
end;

procedure TdxFontFormController.ApplyUnderlineWordsOnly;
begin
  SourceOptions.UseUnderlineWordsOnly := not UnderlineWordsOnly.IsNull and
    ((SourceInfo.UnderlineWordsOnly <> UnderlineWordsOnly.Value) or not SourceOptions.UseUnderlineWordsOnly);
  if SourceOptions.UseUnderlineWordsOnly then
    SourceInfo.UnderlineWordsOnly := UnderlineWordsOnly.Value;
end;

constructor TdxFontFormController.Create(const AControllerParameters: TdxFontFormControllerParameters);
begin
  inherited Create;
  FInitUnderlineWordsOnly := True;
  Assert(Assigned(AControllerParameters));
  FSourceCharacterProperties := AControllerParameters.SourceCharacterProperties;
  InitializeController;
end;

procedure TdxFontFormController.InitializeController;
var
  ASourceInfo: TdxCharacterFormattingInfo;
  ASourceOptions: TdxCharacterFormattingOptions;
begin
  ASourceInfo := SourceCharacterProperties.Info;
  ASourceOptions := SourceCharacterProperties.Options;
  AllCaps := TdxNullableBoolean.Create(ASourceOptions.UseAllCaps, ASourceInfo.AllCaps);
  Script := TdxNullableCharacterFormattingScript.Create(ASourceOptions.UseScript, ASourceInfo.Script);
  FontStrikeoutType := TdxNullableStrikeoutType.Create(ASourceOptions.UseFontStrikeoutType, ASourceInfo.FontStrikeoutType);
  FontName := IfThen(ASourceOptions.UseFontName, ASourceInfo.FontName);
  FontBold := TdxNullableBoolean.Create(ASourceOptions.UseFontBold and ASourceOptions.UseFontItalic, ASourceInfo.FontBold);
  FontItalic := TdxNullableBoolean.Create(ASourceOptions.UseFontBold and ASourceOptions.UseFontItalic, ASourceInfo.FontItalic);
  DoubleFontSize := TdxNullableInteger.Create(ASourceOptions.UseDoubleFontSize, ASourceInfo.DoubleFontSize);
  ForeColor := TdxNullableColor.Create(ASourceOptions.UseForeColor, ASourceInfo.ForeColor);
  UnderlineColor := TdxNullableColor.Create(ASourceOptions.UseUnderlineColor, ASourceInfo.UnderlineColor);
  FontUnderlineType := TdxNullableUnderlineType.Create(ASourceOptions.UseFontUnderlineType, ASourceInfo.FontUnderlineType);
  UnderlineWordsOnly := TdxNullableBoolean.Create(ASourceOptions.UseUnderlineWordsOnly, ASourceInfo.UnderlineWordsOnly);
  Hidden := TdxNullableBoolean.Create(ASourceOptions.UseHidden, ASourceInfo.Hidden);
end;

procedure TdxFontFormController.SetFontUnderline(const AUnderlineType: TdxNullableUnderlineType;
  const AUnderlineWordsOnly: TdxNullableBoolean);
begin
  if (AUnderlineType = FontUnderlineType) and (UnderlineWordsOnly = AUnderlineWordsOnly) then
    Exit;
  if AUnderlineType <> FontUnderlineType then
    FontUnderlineType := AUnderlineType
  else
  begin
    FInitUnderlineWordsOnly := False;
    UnderlineWordsOnly := AUnderlineWordsOnly;
  end;
end;

procedure TdxFontFormController.SetFontUnderlineType(const Value: TdxNullableUnderlineType);
begin
  FFontUnderlineType := Value;
  if (UnderlineWordsOnly = True) and (FFontUnderlineType <> TdxUnderlineType.Single) then
  begin
    FUnderlineWordsOnly.Value := False;
    FPrevFontUnderlineType := FFontUnderlineType;
  end;
end;

procedure TdxFontFormController.SetUnderlineWordsOnly(const Value: TdxNullableBoolean);
begin
  FUnderlineWordsOnly := Value;
  if FUnderlineWordsOnly = True then
  begin
    if FInitUnderlineWordsOnly then
      FPrevFontUnderlineType.Value := TdxUnderlineType.None
    else
      FPrevFontUnderlineType := FFontUnderlineType;
    FFontUnderlineType.Value := TdxUnderlineType.Single;
  end
  else
    if not FPrevFontUnderlineType.IsNull then
      FFontUnderlineType := FPrevFontUnderlineType;
end;

end.
