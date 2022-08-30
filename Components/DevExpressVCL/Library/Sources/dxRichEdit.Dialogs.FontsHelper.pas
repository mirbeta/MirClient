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

unit dxRichEdit.Dialogs.FontsHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Classes, SysUtils, Graphics, Controls, cxEdit, cxListBox, cxTextEdit,
  dxCoreClasses, cxDropDownEdit, cxCheckBox, cxLookAndFeelPainters, dxCoreGraphics,
  dxRichEdit.Options,
  dxRichEdit.Utils.Types,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.View.Core,
  dxRichEdit.Dialogs.ControlHelper,
  dxRichEdit.Dialogs.FontsFormController,
  dxRichEdit.Control;

type
  TdxFontDialogFontStyle = (Regular, Italic, Bold, BoldItalic);

  TdxNullableFontDialogFontStyle = TdxNullableValue<TdxFontDialogFontStyle>;
  TdxNullableColor = TdxNullableValue<TdxAlphaColor>;
  TdxNullableUnderlineType = TdxNullableValue<TdxUnderlineType>;
  TdxNullableStrikeoutType = TdxNullableValue<TdxStrikeoutType>;
  TdxNullableCharacterFormattingScript = TdxNullableValue<TdxCharacterFormattingScript>;

  TdxFontStyleEditHelper = class;
  TdxRichEditFontEffectsHelper = class;

  IdxRichEditFontDialogForm = interface
  ['{2F973A6D-CA94-4E6F-8F97-DDF1DF94650F}']
    function GetFontEffectsEdit: TdxRichEditFontEffectsHelper;
    function GetFontName: TFontName;
    function GetFontNameAllowed: Boolean;
    function GetFontForeColor: TdxNullableColor;
    function GetFontSize: TdxNullableSingle;
    function GetFontStyleEdit: TdxFontStyleEditHelper;
    function GetFontUnderlineType: TdxNullableUnderlineType;
    function GetFontUnderlineColor: TdxNullableColor;
    function GetController: TdxFontFormController;
    function GetOnSomeChildControlEditValueChanged: TNotifyEvent;
    function GetOnFontSizeValidating: TcxEditValidateEvent;
    function GetOnFontStyleValidating: TcxEditValidateEvent;
    procedure SetFontName(const Value: TFontName);
    procedure SetFontNameAllowed(const Value: Boolean);
    procedure SetFontForeColor(const Value: TdxNullableColor);
    procedure SetFontSize(const Value: TdxNullableSingle);
    procedure SetFontUnderlineType(const Value: TdxNullableUnderlineType);
    procedure SetFontUnderlineColor(const Value: TdxNullableColor);
    procedure SetOnSomeChildControlEditValueChanged(const Value: TNotifyEvent);
    procedure SetOnFontSizeValidating(const Value: TcxEditValidateEvent);
    procedure SetOnFontStyleValidating(const Value: TcxEditValidateEvent);
    procedure UpdateRichEditFontControl;
    property FontName: TFontName read GetFontName write SetFontName;
    property FontNameAllowed: Boolean read GetFontNameAllowed write SetFontNameAllowed;
    property FontForeColor: TdxNullableColor read GetFontForeColor write SetFontForeColor;
    property FontSize: TdxNullableSingle read GetFontSize write SetFontSize;
    property FontUnderlineType: TdxNullableUnderlineType read GetFontUnderlineType write SetFontUnderlineType;
    property FontUnderlineColor: TdxNullableColor read GetFontUnderlineColor write SetFontUnderlineColor;
    property FontStyleEdit: TdxFontStyleEditHelper read GetFontStyleEdit;
    property FontEffectsEdit: TdxRichEditFontEffectsHelper read GetFontEffectsEdit;
    property Controller: TdxFontFormController read GetController;
    property OnFontSizeValidating: TcxEditValidateEvent read GetOnFontSizeValidating write SetOnFontSizeValidating;
    property OnFontStyleValidating: TcxEditValidateEvent read GetOnFontStyleValidating write SetOnFontStyleValidating;
    property OnSomeChildControlEditValueChanged: TNotifyEvent read GetOnSomeChildControlEditValueChanged write SetOnSomeChildControlEditValueChanged;
  end;

  { TdxFontStyleEditHelper }

  TdxFontStyleEditHelper = class(TdxCustomListBoxHelper)
  public const
    FontStyleMap: array[TdxFontDialogFontStyle] of TFontStyles =
      ([], [fsItalic], [fsBold], [fsItalic, fsBold]);
  private
    FFontsList: TStrings;
    FOldFontDialogFontStyle: TdxSupportedFontStyles;
    FFontStyles: TdxNullableFontDialogFontStyle;
    FFontFamilyName: TFontName;
    FFontItalic: TdxNullableBoolean;
    FFontBold: TdxNullableBoolean;
    function GetFontStyles: TdxNullableFontDialogFontStyle;
    procedure SetFontStyles(const Value: TdxNullableFontDialogFontStyle);
    procedure SetFontFamilyName(const Value: TFontName);
    function GetFontFamilyName: TFontName;
    procedure SetFontBold(const Value: TdxNullableBoolean);
    procedure SetFontItalic(const Value: TdxNullableBoolean);
  protected
    procedure PopulateFontStyleNames(AItems: TStrings; const AFontFamilyName: TFontName);
    procedure InitItems(const AFontFamilyName: TFontName);
    procedure UpdateControlCore; override;
    procedure EditValueChanged(Sender: TObject);
    function IsFontStyleAllowable(AFontStyle: TdxFontDialogFontStyle): Boolean;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    { IdxBatchUpdateHandler }
    procedure OnBeginUpdate; override;
    procedure OnLastEndUpdate; override;
  public
    constructor Create(AEditControl: TcxTextEdit; AListControl: TcxListBox);
    destructor Destroy; override;
    class function ToSupportedFontStyle(ABold, AItalic: Boolean): TdxSupportedFontStyle; static;

    property FontFamilyName: TFontName read GetFontFamilyName write SetFontFamilyName;
    property FontStyles: TdxNullableFontDialogFontStyle read GetFontStyles write SetFontStyles;
    property FontBold: TdxNullableBoolean read FFontBold write SetFontBold;
    property FontItalic: TdxNullableBoolean read FFontItalic write SetFontItalic;
  end;

  { TdxRichEditFontEffectsHelper }

  TdxRichEditFontEffectsHelper = class(TdxCustomControlHelper)
  private
    FAllCaps: TdxNullableBoolean;
    FHidden: TdxNullableBoolean;
    FStrikeout: TdxNullableStrikeoutType;
    FUnderlineWordsOnly: TdxNullableBoolean;
    FAllCapsCheckBox: TcxCheckBox;
    FDoubleStrikethroughCheckBox: TcxCheckBox;
    FHiddenCheckBox: TcxCheckBox;
    FStrikethroughCheckBox: TcxCheckBox;
    FSubscriptCheckBox: TcxCheckBox;
    FSuperscriptCheckBox: TcxCheckBox;
    FUnderlineWordsOnlyCheckBox: TcxCheckBox;
    FCharacterFormattingDetailedOptions: TdxCharacterFormattingDetailedOptions;
    FScript: TdxNullableCharacterFormattingScript;
    FOnEffectsChanged: TNotifyEvent;
    procedure AllCapsCheckedChanged(Sender: TObject);
    procedure DoubleStrikethroughCheckedChanged(Sender: TObject);
    procedure HiddenCheckedChanged(Sender: TObject);
    procedure StrikethroughCheckedChanged(Sender: TObject);
    procedure SubscriptCheckedChanged(Sender: TObject);
    procedure SuperscriptCheckedChanged(Sender: TObject);
    procedure UnderlineWordsOnlyCheckedChanged(Sender: TObject);
    function CheckStateToNullableBool(ACheckState: TcxCheckBoxState): TdxNullableBoolean;
    procedure SetCharacterFormattingDetailedOptions(const Value: TdxCharacterFormattingDetailedOptions);
    procedure SetStrikeout(const Value: TdxNullableStrikeoutType);
    procedure SetScript(const Value: TdxNullableCharacterFormattingScript);
    procedure SetAllCaps(const Value: TdxNullableBoolean);
    procedure SetHidden(const Value: TdxNullableBoolean);
    procedure SetUnderlineWordsOnly(const Value: TdxNullableBoolean);
  protected
    procedure EffectsChanged;
    procedure DoEffectsChanged;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    procedure UpdateControl; override;
    procedure UpdateControlCore; override;
    procedure UpdateStrikethroughAndDoubleStrikethroughCheckEdits;
    procedure UpdateSuperscriptAndSubscriptCheckEdits;
    procedure UpdateAllCapsCheckEdit;
    procedure UpdateRestrictions;
    procedure UpdateCheckEdit(ACheckBox: TcxCheckBox; const AState: TdxNullableBoolean);
    { IdxBatchUpdateHandler }
    procedure OnLastEndUpdate; override;
  public
    constructor Create(const AAllCaps, ADoubleStrikethrough, AHidden, AStrikethrough, ASubscript, ASuperscript, AUnderlineWordsOnly: TcxCheckBox);
    property CharacterFormattingDetailedOptions: TdxCharacterFormattingDetailedOptions read FCharacterFormattingDetailedOptions write SetCharacterFormattingDetailedOptions;

    property AllCaps: TdxNullableBoolean read FAllCaps write SetAllCaps;
    property Hidden: TdxNullableBoolean read FHidden write SetHidden;
    property Strikeout: TdxNullableStrikeoutType read FStrikeout write SetStrikeout;
    property Script: TdxNullableCharacterFormattingScript read FScript write SetScript;
    property UnderlineWordsOnly: TdxNullableBoolean read FUnderlineWordsOnly write SetUnderlineWordsOnly;
    property OnEffectsChanged: TNotifyEvent read FOnEffectsChanged write FOnEffectsChanged;
  end;

  { TdxRichEditFontHelper }

  TdxRichEditFontHelper = class(TdxCustomControlHelper)
  private
    FOwner: IdxRichEditFontDialogForm;
    FRichEditControl: TdxCustomRichEditControl;
    FOnFontControlChanged: TNotifyEvent;
    FDeferredFontControlChanged: Boolean;
    function GetAllCaps: TdxNullableBoolean;
    function GetFontBold: TdxNullableBoolean;
    function GetFontForeColor: TdxNullableColor;
    function GetFontItalic: TdxNullableBoolean;
    function GetFontName: string; inline;
    function GetFontNameAllowed: Boolean;
    function GetFontSize: TdxNullableSingle;
    function GetFontUnderlineColor: TdxNullableColor;
    function GetFontUnderlineType: TdxNullableUnderlineType;
    function GetHidden: TdxNullableBoolean;
    function GetScript: TdxNullableCharacterFormattingScript;
    function GetStrikeout: TdxNullableStrikeoutType;
    function GetUnderlineWordsOnly: TdxNullableBoolean;
    procedure SetAllCaps(Value: TdxNullableBoolean);
    procedure SetFontBold(Value: TdxNullableBoolean);
    procedure SetFontForeColor(Value: TdxNullableColor);
    procedure SetFontItalic(Value: TdxNullableBoolean);
    procedure SetFontName(const Value: string);
    procedure SetFontNameAllowed(const Value: Boolean);
    procedure SetFontSize(Value: TdxNullableSingle);
    procedure SetFontUnderlineColor(Value: TdxNullableColor);
    procedure SetFontUnderlineType(Value: TdxNullableUnderlineType);
    procedure SetHidden(Value: TdxNullableBoolean);
    procedure SetProperty(AProc: TProc);
    procedure SetRichEditControl(const Value: TdxCustomRichEditControl);
    procedure SetScript(Value: TdxNullableCharacterFormattingScript);
    procedure SetStrikeout(Value: TdxNullableStrikeoutType);
    procedure SetUnderlineWordsOnly(Value: TdxNullableBoolean);
  protected
    FSubscribeCount: Integer;
    procedure FontControlChanged;
    procedure FontSizeValidating(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure FontStyleValidating(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
    procedure SomeChildControlEditValueChanged(Sender: TObject);
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;

    { IdxBatchUpdateHandler }
    procedure OnFirstBeginUpdate; override;
    procedure OnLastEndUpdate; override;
  public
    constructor Create(const AOwner: IdxRichEditFontDialogForm);

    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure CancelUpdate; override;
    procedure DoFontControlChanged;

    property RichEditControl: TdxCustomRichEditControl read FRichEditControl write SetRichEditControl;
    property FontName: string read GetFontName write SetFontName;
    property FontNameAllowed: Boolean read GetFontNameAllowed write SetFontNameAllowed;
    property FontBold: TdxNullableBoolean read GetFontBold write SetFontBold;
    property FontItalic: TdxNullableBoolean read GetFontItalic write SetFontItalic;
    property FontForeColor: TdxNullableColor read GetFontForeColor write SetFontForeColor;
    property FontUnderlineType: TdxNullableUnderlineType read GetFontUnderlineType write SetFontUnderlineType;
    property FontUnderlineColor: TdxNullableColor read GetFontUnderlineColor write SetFontUnderlineColor;
    property AllCaps: TdxNullableBoolean read GetAllCaps write SetAllCaps;
    property Hidden: TdxNullableBoolean read GetHidden write SetHidden;
    property UnderlineWordsOnly: TdxNullableBoolean read GetUnderlineWordsOnly write SetUnderlineWordsOnly;
    property Strikeout: TdxNullableStrikeoutType read GetStrikeout write SetStrikeout;
    property Script: TdxNullableCharacterFormattingScript read GetScript write SetScript;
    property FontSize: TdxNullableSingle read GetFontSize write SetFontSize;

    property OnFontControlChanged: TNotifyEvent read FOnFontControlChanged write FOnFontControlChanged;
  end;

  { TdxRichEditFontSizeHelper }

  TdxRichEditFontNameHelper = class(TdxCustomListBoxHelper)
  public
    procedure PopulateItems;
    function Validate: Boolean; override;
  end;

  { TdxRichEditFontSizeHelper }

  TdxRichEditFontSizeHelper = class(TdxCustomListBoxHelper)
  private
    FRichEditControl: IdxRichEditControl;
    procedure SetRichEditControl(const Value: IdxRichEditControl);
  protected
    procedure PopulateItems;
  public
    property RichEditControl: IdxRichEditControl read FRichEditControl write SetRichEditControl;
    class function TryGetHalfSizeValue(const AEditValue: Variant; out AValue: Integer): Boolean; static;
    class function IsValidFontSize(AFontSize: Integer): Boolean; static;
  end;

implementation

uses
  Math, Variants, cxVariants, cxClasses, dxCore,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Platform.Win.FontCache,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.Dialogs.EditStyleHelper, dxGDIPlusAPI, dxGDIPlusClasses, Windows;

const
  dxFontStyleNames: array[TdxFontDialogFontStyle] of Pointer =
    (@sdxRichEditFontDialogFontStyleRegular, @sdxRichEditFontDialogFontStyleItalic,
     @sdxRichEditFontDialogFontStyleBold, @sdxRichEditFontDialogFontStyleBoldItalic);

{ TdxFontStyleEditHelper }

procedure TdxFontStyleEditHelper.PopulateFontStyleNames(AItems: TStrings; const AFontFamilyName: TFontName);

  procedure AddItem(AItems: TStrings; AFontStyle: TdxFontDialogFontStyle);
  begin
    AItems.AddObject(cxGetResourceString(dxFontStyleNames[AFontStyle]), TObject(AFontStyle));
  end;

  procedure PopulateByDefaultItems(AItems: TStrings);
  var
    I: TdxFontDialogFontStyle;
  begin
    AItems.BeginUpdate;
    AItems.Clear;
    for I := Low(dxFontStyleNames) to High(dxFontStyleNames) do
      AddItem(AItems, I);
    AItems.EndUpdate;
  end;

var
  AIndex: Integer;
  AFontInfo: TdxTrueTypeFontInfo;
  AFontDialogFontStyle: TdxSupportedFontStyles;
begin
  if AFontFamilyName = '' then
  begin
    PopulateByDefaultItems(AItems);
    Exit;
  end;

  AIndex := FFontsList.IndexOf(AFontFamilyName);
  if AIndex < 0 then
  begin
    PopulateByDefaultItems(AItems);
    Exit;
  end;

  AFontInfo := FFontsList.Objects[AIndex] as TdxTrueTypeFontInfo;
  AFontDialogFontStyle := AFontInfo.StylesInfo.SupportedStyles;
  if AFontDialogFontStyle <> FOldFontDialogFontStyle then
  begin
    AItems.BeginUpdate;
    AItems.Clear;
    if TdxSupportedFontStyle.Regular in AFontDialogFontStyle then
      AddItem(AItems, TdxFontDialogFontStyle.Regular);

    if TdxSupportedFontStyle.Bold in AFontDialogFontStyle then
      if not (TdxSupportedFontStyle.Regular in AFontDialogFontStyle) then
        AddItem(AItems, TdxFontDialogFontStyle.Regular)
      else
        AddItem(AItems, TdxFontDialogFontStyle.Bold);

    if TdxSupportedFontStyle.Italic in AFontDialogFontStyle then
      if not (TdxSupportedFontStyle.Regular in AFontDialogFontStyle) then
        AddItem(AItems, TdxFontDialogFontStyle.Regular)
      else
        AddItem(AItems, TdxFontDialogFontStyle.Italic);

    if TdxSupportedFontStyle.BoldItalic in AFontDialogFontStyle then
      AddItem(AItems, TdxFontDialogFontStyle.BoldItalic);
    FOldFontDialogFontStyle := AFontDialogFontStyle;
    AItems.EndUpdate;
  end;
end;

function TdxFontStyleEditHelper.GetFontFamilyName: TFontName;
begin
  Result := FFontFamilyName;
end;

function TdxFontStyleEditHelper.GetFontStyles: TdxNullableFontDialogFontStyle;
begin
  if ListControl.ItemIndex < 0 then
    Result := TdxNullableFontDialogFontStyle.Null
  else
    Result := TdxNullableFontDialogFontStyle.Create(ListControl.Enabled,
      TdxFontDialogFontStyle(ListControl.Items.Objects[ListControl.ItemIndex]));
end;

procedure TdxFontStyleEditHelper.InitItems(const AFontFamilyName: TFontName);
begin
  ListControl.Items.BeginUpdate;
  try
    PopulateFontStyleNames(ListControl.Items, AFontFamilyName);
  finally
    ListControl.Items.EndUpdate;
  end;
end;

function TdxFontStyleEditHelper.IsFontStyleAllowable(AFontStyle: TdxFontDialogFontStyle): Boolean;
begin
  Result := ListControl.Items.IndexOfObject(TObject(AFontStyle)) >= 0;
end;

procedure TdxFontStyleEditHelper.OnBeginUpdate;
begin
  FDeferredUpdateControl := False;
end;

constructor TdxFontStyleEditHelper.Create(AEditControl: TcxTextEdit; AListControl: TcxListBox);
begin
  inherited Create(AEditControl, AListControl);
  FFontsList := TdxGdiFontCache.CreateSystemTrueTypeFonts;
end;

destructor TdxFontStyleEditHelper.Destroy;
begin
  FFontsList.Free;
  inherited Destroy;
end;

class function TdxFontStyleEditHelper.ToSupportedFontStyle(ABold, AItalic: Boolean): TdxSupportedFontStyle;
begin
  if ABold and AItalic then
    Result := TdxSupportedFontStyle.BoldItalic
  else
    if ABold then
      Result := TdxSupportedFontStyle.Bold
    else
      if AItalic then
        Result := TdxSupportedFontStyle.Italic
      else
        Result := TdxSupportedFontStyle.Regular;
end;

procedure TdxFontStyleEditHelper.EditValueChanged(Sender: TObject);
var
  AFontStyle: TdxFontDialogFontStyle;
  AEditValue: TdxNullableFontDialogFontStyle;
begin
  AEditValue := FontStyles;
  if AEditValue.IsNull then
  begin
    FontItalic.Reset;
    FontBold.Reset;
  end
  else
  begin
    AFontStyle := AEditValue.Value;
    FontItalic := AFontStyle in [TdxFontDialogFontStyle.Italic, TdxFontDialogFontStyle.BoldItalic];
    FontBold := AFontStyle in [TdxFontDialogFontStyle.Bold, TdxFontDialogFontStyle.BoldItalic];
  end;
  inherited ListControlClick(Sender);
end;

procedure TdxFontStyleEditHelper.OnLastEndUpdate;
begin
  if FDeferredUpdateControl then
    UpdateControlCore;
end;

procedure TdxFontStyleEditHelper.SetFontBold(const Value: TdxNullableBoolean);
begin
  if FFontBold = Value then
    Exit;
  FFontBold := Value;
  InitItems(FFontFamilyName);
  UpdateControl;
end;

procedure TdxFontStyleEditHelper.SetFontFamilyName(const Value: TFontName);
begin
  FFontFamilyName := Value;
  InitItems(FFontFamilyName);
  UpdateControl;
end;

procedure TdxFontStyleEditHelper.SetFontItalic(const Value: TdxNullableBoolean);
begin
  if FFontItalic = Value then
    Exit;
  FFontItalic := Value;
  InitItems(FFontFamilyName);
  UpdateControl;
end;

procedure TdxFontStyleEditHelper.SetFontStyles(const Value: TdxNullableFontDialogFontStyle);
var
  AIndex: Integer;
begin
  if Value.IsNull then
    AIndex := -1
  else
  begin
    AIndex := ListControl.Items.Count - 1;
    while AIndex >= 0 do
    begin
      if TdxFontDialogFontStyle(ListControl.Items.Objects[AIndex]) = Value.Value then
        Break;
      Dec(AIndex);
    end;
  end;
  ListControl.ItemIndex := AIndex;
  DoUpdateEditControl;
  FFontStyles := Value;
end;

procedure TdxFontStyleEditHelper.SubscribeControlsEvents;
begin
  inherited SubscribeControlsEvents;
  ListControl.OnClick := EditValueChanged;
end;

procedure TdxFontStyleEditHelper.UnsubscribeControlsEvents;
begin
  inherited UnSubscribeControlsEvents;
  ListControl.OnClick := nil;
end;

procedure TdxFontStyleEditHelper.UpdateControlCore;
var
  AFontStyles: TdxNullableFontDialogFontStyle;
begin
  if (FontItalic = True) and (FontBold = True) and IsFontStyleAllowable(TdxFontDialogFontStyle.BoldItalic) then
    AFontStyles := TdxFontDialogFontStyle.BoldItalic
  else
    if (FontItalic = True) and IsFontStyleAllowable(TdxFontDialogFontStyle.Italic) then
      AFontStyles := TdxFontDialogFontStyle.Italic
    else
      if (FontBold = True) and IsFontStyleAllowable(TdxFontDialogFontStyle.Bold) then
        AFontStyles := TdxFontDialogFontStyle.Bold
      else
        if (FontItalic = False) and (FontBold = False) and IsFontStyleAllowable(TdxFontDialogFontStyle.Regular) then
          AFontStyles := TdxFontDialogFontStyle.Regular
        else
          AFontStyles := TdxNullableFontDialogFontStyle.Null;
  FontStyles := AFontStyles
end;

{ TdxRichEditFontEffectsHelper }

procedure TdxRichEditFontEffectsHelper.AllCapsCheckedChanged(Sender: TObject);
begin
  AllCaps := CheckStateToNullableBool(FAllCapsCheckBox.State);
end;

function TdxRichEditFontEffectsHelper.CheckStateToNullableBool(ACheckState: TcxCheckBoxState): TdxNullableBoolean;
begin
  case ACheckState of
  cbsUnchecked:
    Result := False;
  cbsChecked:
    Result := True;
  else
    Result := TdxNullableBoolean.Null;
  end;
end;

constructor TdxRichEditFontEffectsHelper.Create(const AAllCaps, ADoubleStrikethrough, AHidden, AStrikethrough, ASubscript,
  ASuperscript, AUnderlineWordsOnly: TcxCheckBox);
begin
  inherited Create;
  FAllCapsCheckBox := AAllCaps;
  FDoubleStrikethroughCheckBox := ADoubleStrikethrough;
  FHiddenCheckBox := AHidden;
  FStrikethroughCheckBox := AStrikethrough;
  FSubscriptCheckBox := ASubscript;
  FSuperscriptCheckBox := ASuperscript;
  FUnderlineWordsOnlyCheckBox := AUnderlineWordsOnly;

  FScript := TdxCharacterFormattingScript.Normal;
  FStrikeout := TdxStrikeoutType.None;
  FAllCaps := False;
  FUnderlineWordsOnly := False;
  FHidden := false;

  SubscribeControlsEvents;
  UpdateControl;
end;

procedure TdxRichEditFontEffectsHelper.DoEffectsChanged;
begin
  if Assigned(FOnEffectsChanged) then
    FOnEffectsChanged(Self);
end;

procedure TdxRichEditFontEffectsHelper.DoubleStrikethroughCheckedChanged(Sender: TObject);
begin
  case FDoubleStrikethroughCheckBox.State of
    cbsUnchecked:
      Strikeout := TdxStrikeoutType.None;
    cbsChecked:
      Strikeout := TdxStrikeoutType.Double;
    cbsGrayed:
      Strikeout := TdxNullableStrikeoutType.Null;
  end;
end;

procedure TdxRichEditFontEffectsHelper.EffectsChanged;
begin
  if IsUpdateLocked then
    FDeferredUpdateControl := True
  else
    DoEffectsChanged;
end;

procedure TdxRichEditFontEffectsHelper.HiddenCheckedChanged(Sender: TObject);
begin
  Hidden := CheckStateToNullableBool(FHiddenCheckBox.State);
end;

procedure TdxRichEditFontEffectsHelper.OnLastEndUpdate;
begin
  if FDeferredUpdateControl then
    DoEffectsChanged;
  SubscribeControlsEvents;
end;

procedure TdxRichEditFontEffectsHelper.SetAllCaps(const Value: TdxNullableBoolean);
begin
  if FAllCaps = Value then
    Exit;
  FAllCaps := Value;
  UpdateControl;
  EffectsChanged;
end;

procedure TdxRichEditFontEffectsHelper.SetCharacterFormattingDetailedOptions(
  const Value: TdxCharacterFormattingDetailedOptions);
begin
  if FCharacterFormattingDetailedOptions = Value then
    Exit;
  FCharacterFormattingDetailedOptions := Value;
  UpdateControl;
end;

procedure TdxRichEditFontEffectsHelper.SetHidden(const Value: TdxNullableBoolean);
begin
  if FHidden = Value then
    Exit;
  FHidden := Value;
  UpdateControl;
  EffectsChanged;
end;

procedure TdxRichEditFontEffectsHelper.SetScript(const Value: TdxNullableCharacterFormattingScript);
begin
  if FScript = Value then
    Exit;
  FScript := Value;
  UpdateControl;
  EffectsChanged;
end;

procedure TdxRichEditFontEffectsHelper.SetStrikeout(const Value: TdxNullableStrikeoutType);
begin
  if FStrikeout = Value then
    Exit;
  FStrikeout := Value;
  UpdateControl;
  EffectsChanged;
end;

procedure TdxRichEditFontEffectsHelper.SetUnderlineWordsOnly(const Value: TdxNullableBoolean);
begin
  if FUnderlineWordsOnly = Value then
    Exit;
  FUnderlineWordsOnly := Value;
  UpdateControl;
  EffectsChanged;
end;

procedure TdxRichEditFontEffectsHelper.StrikethroughCheckedChanged(Sender: TObject);
begin
  case FStrikethroughCheckBox.State of
    cbsUnchecked:
      Strikeout := TdxStrikeoutType.None;
    cbsChecked:
      Strikeout := TdxStrikeoutType.Single;
    cbsGrayed:
      Strikeout := TdxNullableStrikeoutType.Null;
  end;
end;

procedure TdxRichEditFontEffectsHelper.SubscribeControlsEvents;
begin
  FStrikethroughCheckBox.Properties.OnChange := StrikethroughCheckedChanged;
  FDoubleStrikethroughCheckBox.Properties.OnChange := DoubleStrikethroughCheckedChanged;
  FSuperscriptCheckBox.Properties.OnChange := SuperscriptCheckedChanged;
  FSubscriptCheckBox.Properties.OnChange := SubscriptCheckedChanged;
  FAllCapsCheckBox.Properties.OnChange := AllCapsCheckedChanged;
  FHiddenCheckBox.Properties.OnChange := HiddenCheckedChanged;
  FUnderlineWordsOnlyCheckBox.Properties.OnChange := UnderlineWordsOnlyCheckedChanged;
end;

procedure TdxRichEditFontEffectsHelper.SubscriptCheckedChanged(Sender: TObject);
begin
  case FSubscriptCheckBox.State of
    cbsUnchecked:
      Script := TdxCharacterFormattingScript.Normal;
    cbsChecked:
      Script := TdxCharacterFormattingScript.Subscript;
    cbsGrayed:
      Script := TdxNullableCharacterFormattingScript.Null;
  end;
end;

procedure TdxRichEditFontEffectsHelper.SuperscriptCheckedChanged(Sender: TObject);
begin
  case FSuperscriptCheckBox.State of
    cbsUnchecked:
      Script := TdxCharacterFormattingScript.Normal;
    cbsChecked:
      Script := TdxCharacterFormattingScript.Superscript;
    cbsGrayed:
      Script := TdxNullableCharacterFormattingScript.Null;
  end;
end;

procedure TdxRichEditFontEffectsHelper.UnderlineWordsOnlyCheckedChanged(Sender: TObject);
begin
  UnderlineWordsOnly := CheckStateToNullableBool(FUnderlineWordsOnlyCheckBox.State);
end;

procedure TdxRichEditFontEffectsHelper.UnsubscribeControlsEvents;
begin
  FStrikethroughCheckBox.Properties.OnChange := nil;
  FDoubleStrikethroughCheckBox.Properties.OnChange := nil;
  FSuperscriptCheckBox.Properties.OnChange := nil;
  FSubscriptCheckBox.Properties.OnChange := nil;
  FAllCapsCheckBox.Properties.OnChange := nil;
  FHiddenCheckBox.Properties.OnChange := nil;
  FUnderlineWordsOnlyCheckBox.Properties.OnChange := nil;
end;

procedure TdxRichEditFontEffectsHelper.UpdateAllCapsCheckEdit;
begin
  UpdateCheckEdit(FAllCapsCheckBox, AllCaps);
end;

procedure TdxRichEditFontEffectsHelper.UpdateCheckEdit(ACheckBox: TcxCheckBox; const AState: TdxNullableBoolean);
begin
  if AState.IsNull then
  begin
    ACheckBox.Properties.AllowGrayed := True;
    ACheckBox.State := cbsGrayed;
  end
  else
    if AState.Value then
      ACheckBox.State := cbsChecked
    else
      ACheckBox.State := cbsUnchecked;
end;

procedure TdxRichEditFontEffectsHelper.UpdateControl;
begin
  BeginUpdate;
  UpdateControlCore;
  EndUpdate;
end;

procedure TdxRichEditFontEffectsHelper.UpdateControlCore;
begin
  UpdateStrikethroughAndDoubleStrikethroughCheckEdits;
  UpdateSuperscriptAndSubscriptCheckEdits;
  UpdateAllCapsCheckEdit;
  UpdateCheckEdit(FHiddenCheckBox, Hidden);
  UpdateCheckEdit(FUnderlineWordsOnlyCheckBox, UnderlineWordsOnly);
  UpdateRestrictions;
end;

procedure TdxRichEditFontEffectsHelper.UpdateRestrictions;
begin
  if CharacterFormattingDetailedOptions = nil then
    Exit;
  FAllCapsCheckBox.Enabled := CharacterFormattingDetailedOptions.AllCapsAllowed;
  FStrikethroughCheckBox.Enabled := CharacterFormattingDetailedOptions.StrikeoutWordsOnlyAllowed;
  FUnderlineWordsOnlyCheckBox.Enabled := CharacterFormattingDetailedOptions.UnderlineWordsOnlyAllowed;
  FSubscriptCheckBox.Enabled := CharacterFormattingDetailedOptions.ScriptAllowed;
  FSuperscriptCheckBox.Enabled := CharacterFormattingDetailedOptions.ScriptAllowed;
  FHiddenCheckBox.Enabled := CharacterFormattingDetailedOptions.HiddenAllowed;
  FStrikethroughCheckBox.Enabled := CharacterFormattingDetailedOptions.FontStrikeoutAllowed;
  FDoubleStrikethroughCheckBox.Enabled := CharacterFormattingDetailedOptions.FontStrikeoutAllowed;
end;

procedure TdxRichEditFontEffectsHelper.UpdateStrikethroughAndDoubleStrikethroughCheckEdits;
begin
  if Strikeout.IsNull then
  begin
    FStrikethroughCheckBox.Properties.AllowGrayed := True;
    FDoubleStrikethroughCheckBox.Properties.AllowGrayed := True;
    FStrikethroughCheckBox.State := cbsGrayed;
    FDoubleStrikethroughCheckBox.State := cbsGrayed;
  end
  else
  case Strikeout.Value of
    TdxStrikeoutType.Single:
      begin
        FStrikethroughCheckBox.State := cbsChecked;
        FDoubleStrikethroughCheckBox.State := cbsUnchecked;
      end;
    TdxStrikeoutType.Double:
      begin
        FStrikethroughCheckBox.State := cbsUnchecked;
        FDoubleStrikethroughCheckBox.State := cbsChecked;
      end;
    TdxStrikeoutType.None:
      begin
        FStrikethroughCheckBox.State := cbsUnchecked;
        FDoubleStrikethroughCheckBox.State := cbsUnchecked;
      end
    else
      begin
        FStrikethroughCheckBox.State := cbsGrayed;
        FDoubleStrikethroughCheckBox.State := cbsGrayed;
      end
  end;
end;

procedure TdxRichEditFontEffectsHelper.UpdateSuperscriptAndSubscriptCheckEdits;
begin
  if Script.IsNull then
  begin
    FSuperscriptCheckBox.Properties.AllowGrayed := True;
    FSubscriptCheckBox.Properties.AllowGrayed := True;
    FSuperscriptCheckBox.State := cbsGrayed;
    FSubscriptCheckBox.State := cbsGrayed;
  end
  else
  case Script.Value of
    TdxCharacterFormattingScript.Superscript:
      begin
        FSuperscriptCheckBox.State := cbsChecked;
        FSubscriptCheckBox.State := cbsUnchecked
      end;
    TdxCharacterFormattingScript.Subscript:
      begin
        FSuperscriptCheckBox.State := cbsUnchecked;
        FSubscriptCheckBox.State := cbsChecked;
      end
    else
      begin
        FSuperscriptCheckBox.State := cbsUnchecked;
        FSubscriptCheckBox.State := cbsUnchecked;
      end;
  end;
end;

{ TdxRichEditFontHelper }

procedure TdxRichEditFontHelper.BeginUpdate;
begin
  inherited BeginUpdate;
  FOwner.FontStyleEdit.BeginUpdate;
  FOwner.FontEffectsEdit.BeginUpdate;
end;

procedure TdxRichEditFontHelper.CancelUpdate;
begin
  inherited CancelUpdate;
  FOwner.FontStyleEdit.CancelUpdate;
  FOwner.FontEffectsEdit.CancelUpdate;
end;

procedure TdxRichEditFontHelper.DoFontControlChanged;
begin
  if Assigned(FOnFontControlChanged) then
    FOnFontControlChanged(Self);
end;

procedure TdxRichEditFontHelper.EndUpdate;
begin
  FOwner.FontStyleEdit.EndUpdate;
  FOwner.FontEffectsEdit.EndUpdate;
  inherited EndUpdate;
end;

procedure TdxRichEditFontHelper.FontControlChanged;
begin
  if IsUpdateLocked then
    FDeferredFontControlChanged := True
  else
    DoFontControlChanged;
end;

procedure TdxRichEditFontHelper.FontSizeValidating(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
var
  AText: string;
begin
  Error := False;
  AText := '';
  if VarIsSoftNull(DisplayValue) then
    Exit;
  Error := not TdxEditStyleHelper.IsFontSizeValid(DisplayValue, AText);
  if Error then
    ErrorText := AText;
end;

procedure TdxRichEditFontHelper.FontStyleValidating(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
begin
  Error := False;
  if VarIsSoftNull(DisplayValue) then
    Exit;
  Error := FOwner.FontStyleEdit.ListControl.Items.IndexOf(DisplayValue) < 0;
end;

function TdxRichEditFontHelper.GetAllCaps: TdxNullableBoolean;
begin
  Result := FOwner.FontEffectsEdit.AllCaps;
end;

function TdxRichEditFontHelper.GetFontBold: TdxNullableBoolean;
begin
  Result := FOwner.FontStyleEdit.FontBold;
end;

function TdxRichEditFontHelper.GetFontForeColor: TdxNullableColor;
begin
  Result := FOwner.FontForeColor;
end;

function TdxRichEditFontHelper.GetFontItalic: TdxNullableBoolean;
begin
  Result := FOwner.FontStyleEdit.FontItalic;
end;

function TdxRichEditFontHelper.GetFontName: string;
begin
  Result := FOwner.FontName;
end;

function TdxRichEditFontHelper.GetFontNameAllowed: Boolean;
begin
  Result := FOwner.FontNameAllowed;
end;

function TdxRichEditFontHelper.GetFontSize: TdxNullableSingle;
begin
  Result := FOwner.FontSize;
end;

function TdxRichEditFontHelper.GetFontUnderlineColor: TdxNullableColor;
begin
  Result := FOwner.FontUnderlineColor;
end;

function TdxRichEditFontHelper.GetFontUnderlineType: TdxNullableUnderlineType;
begin
  Result := FOwner.FontUnderlineType
end;

function TdxRichEditFontHelper.GetHidden: TdxNullableBoolean;
begin
  Result := FOwner.FontEffectsEdit.Hidden;
end;

function TdxRichEditFontHelper.GetScript: TdxNullableCharacterFormattingScript;
begin
  Result := FOwner.FontEffectsEdit.Script;
end;

function TdxRichEditFontHelper.GetStrikeout: TdxNullableStrikeoutType;
begin
  Result := FOwner.FontEffectsEdit.Strikeout;
end;

function TdxRichEditFontHelper.GetUnderlineWordsOnly: TdxNullableBoolean;
begin
  Result := FOwner.FontEffectsEdit.UnderlineWordsOnly;
end;

procedure TdxRichEditFontHelper.OnFirstBeginUpdate;
begin
  inherited OnFirstBeginUpdate;
  FDeferredFontControlChanged := False;
end;

procedure TdxRichEditFontHelper.OnLastEndUpdate;
begin
  if FDeferredUpdateControl then
    FOwner.UpdateRichEditFontControl;
  if FDeferredFontControlChanged then
    DoFontControlChanged;
  SubscribeControlsEvents;
end;

procedure TdxRichEditFontHelper.SetAllCaps(Value: TdxNullableBoolean);
begin
  SetProperty(procedure
    begin
      FOwner.FontEffectsEdit.AllCaps := Value;
    end);
end;

procedure TdxRichEditFontHelper.SetFontBold(Value: TdxNullableBoolean);
begin
  SetProperty(procedure
    begin
      FOwner.FontStyleEdit.FontBold := Value;
    end);
end;

procedure TdxRichEditFontHelper.SetFontForeColor(Value: TdxNullableColor);
begin
  SetProperty(procedure
    begin
      FOwner.FontForeColor := Value;
    end);
end;

procedure TdxRichEditFontHelper.SetFontItalic(Value: TdxNullableBoolean);
begin
  SetProperty(procedure
    begin
      FOwner.FontStyleEdit.FontItalic := Value;
    end);
end;

procedure TdxRichEditFontHelper.SetFontName(const Value: string);
begin
  SetProperty(procedure
    begin
      FOwner.FontName := Value;
      FOwner.FontStyleEdit.FontFamilyName := Value;
    end);
end;

procedure TdxRichEditFontHelper.SetFontNameAllowed(const Value: Boolean);
begin
  BeginUpdate;
  FOwner.FontNameAllowed := Value;
  EndUpdate;
end;

procedure TdxRichEditFontHelper.SetFontSize(Value: TdxNullableSingle);
begin
  SetProperty(procedure
    begin
      FOwner.FontSize := Value;
    end);
end;

procedure TdxRichEditFontHelper.SetFontUnderlineColor(Value: TdxNullableColor);
begin
  SetProperty(procedure
    begin
      FOwner.FontUnderlineColor := Value;
    end);
end;

procedure TdxRichEditFontHelper.SetFontUnderlineType(Value: TdxNullableUnderlineType);
begin
  SetProperty(procedure
    begin
      FOwner.FontUnderlineType := Value;
    end);
end;

procedure TdxRichEditFontHelper.SetHidden(Value: TdxNullableBoolean);
begin
  SetProperty(procedure
    begin
      FOwner.FontEffectsEdit.Hidden := Value;
    end);
end;

procedure TdxRichEditFontHelper.SetProperty(AProc: TProc);
begin
  BeginUpdate;
  try
    AProc;
    UpdateControl;
    FontControlChanged;
  finally
    EndUpdate;
  end;
end;

procedure TdxRichEditFontHelper.SetRichEditControl(const Value: TdxCustomRichEditControl);
begin
  SetProperty(procedure
    begin
      FRichEditControl := Value;
    end);
end;

procedure TdxRichEditFontHelper.SetScript(Value: TdxNullableCharacterFormattingScript);
begin
  SetProperty(procedure
    begin
      FOwner.FontEffectsEdit.Script := Value;
    end);
end;

procedure TdxRichEditFontHelper.SetStrikeout(Value: TdxNullableStrikeoutType);
begin
  SetProperty(procedure
    begin
      FOwner.FontEffectsEdit.Strikeout := Value;
    end);
end;

procedure TdxRichEditFontHelper.SetUnderlineWordsOnly(Value: TdxNullableBoolean);
begin
  SetProperty(procedure
    begin
      FOwner.FontEffectsEdit.UnderlineWordsOnly := Value;
    end);
end;

procedure TdxRichEditFontHelper.SomeChildControlEditValueChanged(Sender: TObject);
begin
  UpdateControl;
  FontControlChanged;
end;

procedure TdxRichEditFontHelper.SubscribeControlsEvents;
begin
  FOwner.OnSomeChildControlEditValueChanged := SomeChildControlEditValueChanged;
  Inc(FSubscribeCount);
end;

procedure TdxRichEditFontHelper.UnsubscribeControlsEvents;
begin
  if FSubscribeCount > 1 then
  begin
    Dec(FSubscribeCount);
    if FSubscribeCount > 0 then
      Exit;
  end;
  FOwner.OnSomeChildControlEditValueChanged := nil;
end;

constructor TdxRichEditFontHelper.Create(const AOwner: IdxRichEditFontDialogForm);
begin
  inherited Create;
  FOwner := AOwner;
  FOwner.OnFontSizeValidating := FontSizeValidating;
  FOwner.OnFontStyleValidating := FontStyleValidating;

  SubscribeControlsEvents;
end;

{ TdxRichEditFontSizeHelper }

class function TdxRichEditFontSizeHelper.IsValidFontSize(AFontSize: Integer): Boolean;
begin
  Result := (AFontSize >= TdxPredefinedFontSizeCollection.MinFontSize) and (AFontSize <= TdxPredefinedFontSizeCollection.MaxFontSize);
end;

procedure TdxRichEditFontSizeHelper.PopulateItems;
var
  I: Integer;
  AFontSizes: TdxPredefinedFontSizeCollection;
begin
  if ListControl.Items.Count > 0 then
    Exit;
  ListControl.Items.Clear;
  if Assigned(RichEditControl) then
  begin
    ListControl.Items.BeginUpdate;
    try
      AFontSizes := RichEditControl.InnerControl.PredefinedFontSizeCollection;
      for I := 0 to AFontSizes.Count - 1 do
        ListControl.Items.Add(IntToStr(AFontSizes[I]));
    finally
      ListControl.Items.EndUpdate;
    end;
  end;
end;

procedure TdxRichEditFontSizeHelper.SetRichEditControl(const Value: IdxRichEditControl);
begin
  UnsubscribeControlsEvents;
  FRichEditControl := Value;
  PopulateItems;
  SubscribeControlsEvents;
end;

class function TdxRichEditFontSizeHelper.TryGetHalfSizeValue(const AEditValue: Variant; out AValue: Integer): Boolean;
var
  AEditFloat: Single;
  AEditText: string;
begin
  AValue := 0;
  if VarIsOrdinal(AEditValue) then
  begin
    AValue := AEditValue * 2;
    Result := True;
  end
  else
  begin
    AEditFloat := 0;
    AEditText := '';
    if VarIsFloat(AEditValue) then
      AEditFloat := AEditValue
    else
      AEditText := AEditValue;
    if ((AEditText <> '') and TryStrToFloat(AEditText, AEditFloat)) or (AEditFloat > 0) then
    begin
      if IsZero(Frac(AEditFloat * 2.0)) then
      begin
        AValue := Trunc(AEditFloat * 2);
        Result := True;
      end
      else
        Result := False;
    end
    else
      Result := False;
  end;
end;

{ TdxRichEditFontNameHelper }

procedure TdxRichEditFontNameHelper.PopulateItems;
var
  AStrings: TStrings;
  AValue: TdxTrueTypeFontInfo;
  I: Integer;
begin
  UnsubscribeControlsEvents;
  AStrings := TdxGdiFontCache.CreateSystemTrueTypeFonts;
  try
    ListControl.Items.BeginUpdate;
    ListControl.Items.Clear;
    for I := 0 to AStrings.Count - 1 do
    begin
      AValue := TdxTrueTypeFontInfo(AStrings.Objects[I]);
      ListControl.Items.AddObject(AValue.FontName, AValue);
    end;
    ListControl.Items.EndUpdate;
  finally
    AStrings.Free;
    SubscribeControlsEvents;
  end;
end;

function TdxRichEditFontNameHelper.Validate: Boolean;
begin
  Result := True;
end;

end.

