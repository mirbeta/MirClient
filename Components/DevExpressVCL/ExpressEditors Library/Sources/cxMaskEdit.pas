{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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
unit cxMaskEdit;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Clipbrd,
  cxClasses, cxControls, cxContainer, cxDataStorage, cxDataUtils, cxEdit,
  cxTextEdit, cxEditConsts, cxRegExpr, cxStandardMask, cxFilterControlUtils;

type
  TcxEditMask = type string;
  TcxEditMaskKind = (emkStandard, emkRegExpr, emkRegExprEx);

  TcxCustomMaskEdit = class;
  TcxCustomMaskEditProperties = class;

  { EcxMaskEditError }

  EcxMaskEditError = class(EcxEditError);

  { TcxMaskEditCustomMode }

  TcxMaskEditCustomMode = class
  private
    FCharCase: TEditCharCase;
    FEchoMode: TcxEditEchoMode;
    FEditMask: string;
    FProperties: TcxCustomMaskEditProperties;
    function GetProperties: TcxCustomMaskEditProperties;
  protected
    FClipboardTextLength: Integer;
    FEdit: TcxCustomMaskEdit;
    FNeedUpdateEditValue: Boolean;
    procedure ClearText;
    function GetMaskKind: TcxEditMaskKind; virtual;
    function HasEdit: Boolean;
    function ProcessSelText(const Value: string; out ANewText: string;
      out ANewSelStart: Integer): Boolean; virtual;
    property CharCase: TEditCharCase read FCharCase write FCharCase;
    property EchoMode: TcxEditEchoMode read FEchoMode write FEchoMode;
    property EditMask: string read FEditMask;
    property Properties: TcxCustomMaskEditProperties read GetProperties;
  public
    constructor Create(AEdit: TcxCustomMaskEdit; AProperties: TcxCustomMaskEditProperties); virtual;
    procedure AfterPasteFromClipboard; virtual; abstract;
    procedure BeepOnError;
    procedure Compile(AMask: string); virtual; abstract;
    function GetEmptyString: string; virtual; abstract;
    function GetFormattedText(const AText: string; AMatchForBlanksAndLiterals: Boolean = True): string; virtual; abstract;
    procedure GotoEnd; virtual; abstract;
    function IsCursorBegin: Boolean; virtual; abstract;
    function IsCursorEnd: Boolean; virtual; abstract;
    function IsFullValidText(AText: string): Boolean; virtual; abstract;
    procedure LMouseDown; virtual; abstract;
    procedure PrePasteFromClipboard; virtual; abstract;
    function PressBackSpace: Boolean; virtual; abstract;
    function PressDelete: Boolean; virtual; abstract;
    function PressEnd: Boolean; virtual; abstract;
    function PressHome: Boolean; virtual; abstract;
    function PressLeft: Boolean; virtual; abstract;
    function PressRight: Boolean; virtual; abstract;
    function PressSymbol(var ASymbol: Char): Boolean; virtual; abstract;
    procedure SetText(AText: string); virtual; abstract;
    procedure SynchronizeEditValue; virtual;
    procedure UpdateEditValue; virtual; abstract;
    function GetUpdatedText(const AText: string;
      AMatchForBlanksAndLiterals: Boolean = True): string; virtual; abstract;
    property ClipboardTextLength: Integer read FClipboardTextLength write FClipboardTextLength;
  end;

  TcxMaskEditCustomModeClass = class of TcxMaskEditCustomMode;

  { TcxMaskEditStandardMode }

  TcxMaskEditStandardMode = class(TcxMaskEditCustomMode)
  protected
    FMask: TcxStandardMask;
    FSelStart: Integer;
    function GetBlank(APos: Integer): Char; virtual;
    function ProcessSelText(const Value: string; out ANewText: string;
      out ANewSelStart: Integer): Boolean; override;
  public
    constructor Create(AEdit: TcxCustomMaskEdit; AProperties: TcxCustomMaskEditProperties); override;
    destructor Destroy; override;
    procedure AfterPasteFromClipboard; override;
    procedure Compile(AMask: string); override;
    function GetEmptyString: string; override;
    function GetFormattedText(const AText: string; AMatchForBlanksAndLiterals: Boolean = True): string; override;
    procedure GotoEnd; override;
    function IsCursorBegin: Boolean; override;
    function IsCursorEnd: Boolean; override;
    function IsFullValidText(AText: string): Boolean; override;
    procedure LMouseDown; override;
    procedure PrePasteFromClipboard; override;
    function PressBackSpace: Boolean; override;
    function PressDelete: Boolean; override;
    function PressEnd: Boolean; override;
    function PressHome: Boolean; override;
    function PressLeft: Boolean; override;
    function PressRight: Boolean; override;
    function PressSymbol(var ASymbol: Char): Boolean; override;
    procedure SetText(AText: string); override;
    procedure SynchronizeEditValue; override;
    procedure UpdateEditValue; override;
    function GetUpdatedText(const AText: string;
      AMatchForBlanksAndLiterals: Boolean = True): string; override;
  end;

  { TcxMaskEditRegExprMode }

  TcxMaskEditRegExprMode = class(TcxMaskEditCustomMode)
  protected
    FBeginCursor: Boolean;
    FHead: string;
    FRegExpr: TcxRegExpr;
    FSelect: string;
    FTail: string;
    FMouseAction: Boolean;
    procedure ClearTail;
    function CompileRegExpr(ARegExpr: TcxRegExpr): Boolean;
    function NeedReset: Boolean; virtual;
    procedure CursorCorrection; virtual;
    procedure DeleteSelection; virtual;
    function GetMaskKind: TcxEditMaskKind; override;
    function NextTail: Boolean;
    procedure RestoreSelection; virtual;
    procedure Reset(const AText: string);
  public
    constructor Create(AEdit: TcxCustomMaskEdit; AProperties: TcxCustomMaskEditProperties); override;
    destructor Destroy; override;
    procedure AfterPasteFromClipboard; override;
    procedure Compile(AMask: string); override;
    function GetEmptyString: string; override;
    function GetFormattedText(const AText: string; AMatchForBlanksAndLiterals: Boolean = True): string; override;
    procedure GotoEnd; override;
    function IsCursorBegin: Boolean; override;
    function IsCursorEnd: Boolean; override;
    function IsFullValidText(AText: string): Boolean; override;
    procedure LMouseDown; override;
    procedure PrePasteFromClipboard; override;
    function PressBackSpace: Boolean; override;
    function PressDelete: Boolean; override;
    function PressEnd: Boolean; override;
    function PressHome: Boolean; override;
    function PressLeft: Boolean; override;
    function PressRight: Boolean; override;
    function PressSymbol(var ASymbol: Char): Boolean; override;
    procedure SetText(AText: string); override;
    procedure SetRegExprCaseInsensitive;
    procedure SynchronizeEditValue; override;
    procedure UpdateEditValue; override;
    function GetUpdatedText(const AText: string;
      AMatchForBlanksAndLiterals: Boolean = True): string; override;
  end;

  { TcxMaskEditRegExprExMode }

  TcxMaskEditRegExprExMode = class(TcxMaskEditRegExprMode)
  private
    FInternalUpdate: string;
    procedure InternalSymbolUpdate(ASymbol: Char);
  protected
    FDeleteNumber: Integer;
    FNewCursorPos: Integer;
    FUpdate: string;
    procedure Clear;
    procedure CursorCorrection; override;
    procedure DeleteSelection; override;
    function GetMaskKind: TcxEditMaskKind; override;
    procedure RestoreSelection; override;
    procedure SymbolDelete;
    procedure SymbolUpdate(ASymbol: Char);
    procedure UpdateTail;
  public
    constructor Create(AEdit: TcxCustomMaskEdit; AProperties: TcxCustomMaskEditProperties); override;
    destructor Destroy; override;
    procedure AfterPasteFromClipboard; override;
    procedure Compile(AMask: string); override;
    function GetEmptyString: string; override;
    function GetFormattedText(const AText: string; AMatchForBlanksAndLiterals: Boolean = True): string; override;
    procedure GotoEnd; override;
    function IsFullValidText(AText: string): Boolean; override;
    procedure PrePasteFromClipboard; override;
    function PressBackSpace: Boolean; override;
    function PressDelete: Boolean; override;
    function PressEnd: Boolean; override;
    function PressHome: Boolean; override;
    function PressLeft: Boolean; override;
    function PressRight: Boolean; override;
    function PressSymbol(var ASymbol: Char): Boolean; override;
    procedure SetText(AText: string); override;
    procedure UpdateEditValue; override;
  end;

  { TcxCustomMaskEditProperties }

  TcxCustomMaskEditProperties = class(TcxCustomTextEditProperties)
  private
    FAlwaysShowBlanksAndLiterals: Boolean;
    FCaseInsensitive: Boolean;
    FEditMask: TcxEditMask;
    FEmptyString: string;
    FIgnoreMaskBlank: Boolean;
    FMaskKind: TcxEditMaskKind;
    FMaxLengthSetting: Boolean;
    FTempMode: TcxMaskEditCustomMode;
    function CreateMode: TcxMaskEditCustomMode;
    function CreateMode1: TcxMaskEditCustomMode;
    function GetEditMask: TcxEditMask;
    function GetIsMasked: Boolean;
    procedure SetAlwaysShowBlanksAndLiterals(AValue: Boolean);
    procedure SetCaseInsensitive(AValue: Boolean);
    procedure SetEditMask(Value: TcxEditMask);
    procedure SetMaskKind(Value: TcxEditMaskKind);
    function SpaceToken(AToken: Char): Boolean;
    function TestTempEditMask: Boolean;
    function TestTempMaskKind: Boolean;
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    procedure DoChanged; override;
    function GetDisplayFormatOptions: TcxEditDisplayFormatOptions; override;
    procedure MaxLengthChanged; override;
    procedure SetCharCase(Value: TEditCharCase); override;
    function UseLookupData: Boolean; override;

    function EmptyMask(AMask: string): Boolean;
    function GetEmptyString: string;
    function GetModeClass(AMaskKind: TcxEditMaskKind): TcxMaskEditCustomModeClass; virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    class function GetContainerClass: TcxContainerClass; override;
//    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    function IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    procedure DoPrepareDisplayValue(const AEditValue: TcxEditValue; var ADisplayValue: TcxEditValue;
      AEditFocused: Boolean); override;
    procedure ValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
      var AError: Boolean; AEdit: TcxCustomEdit); override;
  public
    property IsMasked: Boolean read GetIsMasked;
    // !!!
    property AlwaysShowBlanksAndLiterals: Boolean
      read FAlwaysShowBlanksAndLiterals write SetAlwaysShowBlanksAndLiterals
      default False;
    property CaseInsensitive: Boolean read FCaseInsensitive
      write SetCaseInsensitive default True;
    property EditMask: TcxEditMask read GetEditMask write SetEditMask;
    property IgnoreMaskBlank: Boolean read FIgnoreMaskBlank
      write FIgnoreMaskBlank default False;
    property MaskKind: TcxEditMaskKind read FMaskKind write SetMaskKind
      default emkStandard;
    property ValidateOnEnter default True;
  end;

  TcxMaskEditProperties = class;

  { TcxCustomMaskEdit }

  TcxCustomMaskEdit = class(TcxCustomTextEdit)
  private
    FInternalTextSelection: Boolean;
    FMode: TcxMaskEditCustomMode;
    FMyMessage: Boolean;
    FShiftOn: Boolean;
    FText: string;
    function GetEditText: TCaption;
    function GetProperties: TcxCustomMaskEditProperties;
    function GetActiveProperties: TcxCustomMaskEditProperties;
    function InternalGetEmptyString: string;
    procedure InternalSetValue(AValue: string);
    function IsCursorBegin: Boolean;
    function IsCursorEnd: Boolean;
    procedure SetEditText(AValue: TCaption);
    procedure SetProperties(Value: TcxCustomMaskEditProperties);
    procedure SendMyKeyDown(Key: Word; Shift: TShiftState);
    procedure SendMyKeyPress(AKey: Char);
  protected
    function CanChangeSelText(const Value: string; out ANewText: string;
      out ANewSelStart: Integer): Boolean; override;
    procedure ChangeHandler(Sender: TObject); override;
    procedure DblClick; override;
    procedure DoEnter; override;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEditKeyPress(var Key: Char); override;
    procedure Initialize; override;
    function InternalGetText: string; override;
    procedure InternalSetDisplayValue(const Value: TcxEditValue); override;
    function InternalSetText(const Value: string): Boolean; override;
    function IsTextInputMode: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure LockInnerEditRepainting; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure SelChange(Sender: TObject); override;
    procedure SetSelText(const Value: TCaption); override;
    procedure SynchronizeDisplayValue; override;
    procedure UndoPerformed; override;
    procedure UnlockInnerEditRepainting; override;
    function CanSynchronizeModeText: Boolean; virtual;
    procedure DirectSetSelLength(AValue: Integer);
    procedure DirectSetSelStart(AValue: Integer);
    function IsCharValidForPos(var AChar: Char; APos: Integer): Boolean; virtual;
    procedure UpdateTextFormatting; virtual;
    property Mode: TcxMaskEditCustomMode read FMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CutToClipboard; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure PasteFromClipboard; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue;
      out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    property ActiveProperties: TcxCustomMaskEditProperties read GetActiveProperties;
    property EditText: TCaption read GetEditText write SetEditText;
    property Properties: TcxCustomMaskEditProperties read GetProperties
      write SetProperties;
  end;

  { TcxMaskEditProperties }

  TcxMaskEditProperties = class(TcxCustomMaskEditProperties)
  published
    property Alignment;
    property AlwaysShowBlanksAndLiterals;
    property AssignedValues;
    property AutoSelect;
    property BeepOnError;
    property CaseInsensitive;
    property CharCase;
    property ClearKey;
    property EchoMode;
    property HideSelection;
    property IgnoreMaskBlank;
    property ImeMode;
    property ImeName;
    property IncrementalSearch;
    property LookupItems;
    property LookupItemsSorted;
    property MaskKind;
    property EditMask;
    property MaxLength;
    property Nullstring;
    property OEMConvert;
    property PasswordChar;
    property ReadOnly;
    property UseLeftAlignmentOnEditing;
    property UseNullString;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnChange;
    property OnEditValueChanged;
    property OnNewLookupDisplayText;
    property OnValidate;
  end;

  { TcxMaskEdit }

  TcxMaskEdit = class(TcxCustomMaskEdit)
  private
    function GetActiveProperties: TcxMaskEditProperties;
    function GetProperties: TcxMaskEditProperties;
    procedure SetProperties(Value: TcxMaskEditProperties);
  public
    function SupportsSpelling: Boolean; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxMaskEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxMaskEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Visible;
    property DragCursor;
    property DragKind;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxFilterMaskEditHelper }

  TcxFilterMaskEditHelper = class(TcxFilterTextEditHelper)
  public
    class function GetFilterEditClass: TcxCustomEditClass; override;
    class function GetSupportedFilterOperators(
      AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass;
      AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
  end;

function IsAlphaChar(ch: Char): Boolean;
//function GetSaveLiteralChars(AMaskEdit: TcxCustomMaskEdit): Boolean;

implementation

uses
  dxCore, cxVariants, Math;

function IsAlphaChar(ch: Char): Boolean;
begin
  Result := IsCharAlpha(ch);
end;

{function GetSaveLiteralChars(AMaskEdit: TcxCustomMaskEdit): Boolean;
begin
  Result := False;
  if AMaskEdit.Mode is TcxMaskEditStandardMode then
    if TcxMaskEditStandardMode(AMaskEdit.Mode).FMask <> nil then
      Result := TcxMaskEditStandardMode(AMaskEdit.Mode).FMask.SaveLiteralCharacters;
end;}

{ TcxMaskEditCustomMode }

constructor TcxMaskEditCustomMode.Create(AEdit: TcxCustomMaskEdit;
  AProperties: TcxCustomMaskEditProperties);
begin
  inherited Create;
  FEdit := AEdit;
  if FEdit = nil then
    FProperties := AProperties;
  FClipboardTextLength := 0;
  FNeedUpdateEditValue := False;
  FCharCase := ecNormal;
  FEchoMode := eemNormal;
end;

procedure TcxMaskEditCustomMode.BeepOnError;
begin
  if Properties.BeepOnError then
    Beep;
end;

procedure TcxMaskEditCustomMode.SynchronizeEditValue;
var
  ADisplayValue: Variant;
begin
  if HasEdit then
  begin
    if FEdit.ModifiedAfterEnter then
      ADisplayValue := GetFormattedText(FEdit.DisplayText)
    else
      FEdit.ActiveProperties.PrepareDisplayValue(FEdit.EditValue, ADisplayValue, FEdit.Focused);
    FEdit.SetInternalDisplayValue(ADisplayValue);
  end;
end;

procedure TcxMaskEditCustomMode.ClearText;
begin
  if HasEdit then
    FEdit.DataBinding.UpdateNotConnectedDBEditDisplayValue;
end;

function TcxMaskEditCustomMode.GetMaskKind: TcxEditMaskKind;
begin
  Result := emkStandard;
end;

function TcxMaskEditCustomMode.HasEdit: Boolean;
begin
  Result := (FEdit <> nil) and not FEdit.PropertiesChangeLocked;
end;

function TcxMaskEditCustomMode.ProcessSelText(const Value: string;
  out ANewText: string; out ANewSelStart: Integer): Boolean;
begin
  Result := False;
end;

function TcxMaskEditCustomMode.GetProperties: TcxCustomMaskEditProperties;
begin
  if FEdit <> nil then
    Result := FEdit.ActiveProperties
  else
    Result := FProperties;
end;

{ TcxMaskEditStandardMode }

constructor TcxMaskEditStandardMode.Create(AEdit: TcxCustomMaskEdit;
  AProperties: TcxCustomMaskEditProperties);
begin
  inherited Create(AEdit, AProperties);

  FMask := TcxStandardMask.Create;
end;

destructor TcxMaskEditStandardMode.Destroy;
begin
  FMask.Free;
  inherited Destroy;
end;

procedure TcxMaskEditStandardMode.AfterPasteFromClipboard;
begin
  if (FEdit.SelStart < Length(FEdit.Text)) and (FMask.Items[FEdit.SelStart] is TcxStandardMaskLiteralItem) then
  begin
    FEdit.FShiftOn := False;
    PressRight;
  end;
end;

procedure TcxMaskEditStandardMode.Compile(AMask: string);
begin
  FEditMask := AMask;
  if Properties.EmptyMask(AMask) then
    Exit;

  FMask.Compile(AMask);

  FNeedUpdateEditValue := not HasEdit;
end;

function TcxMaskEditStandardMode.GetEmptyString: string;
begin
  Result := FMask.EmptyString;
end;

function TcxMaskEditStandardMode.GetFormattedText(const AText: string; AMatchForBlanksAndLiterals: Boolean = True): string;
begin
  Result := AText;
  FMask.Format(Result, True, AMatchForBlanksAndLiterals);
end;

procedure TcxMaskEditStandardMode.GotoEnd;
begin
end;

function TcxMaskEditStandardMode.IsCursorBegin: Boolean;
var
  I: Integer;
  ACount: Integer;
begin
  ACount := 0;
  for I := 0 to FMask.Count - 1 do
  begin
    if FMask.Items[I] is TcxStandardMaskLiteralItem then
      Inc(ACount)
    else
      Break;
  end;
  Result := (FEdit.SelStart <= ACount) and (FEdit.SelLength <= 1) or
    (FEdit.SelStart <= ACount) and (FEdit.CursorPos = FEdit.SelStart);
end;

function TcxMaskEditStandardMode.IsCursorEnd: Boolean;
begin
  Result := FEdit.SelStart = Length(FEdit.EditText);
end;

function TcxMaskEditStandardMode.IsFullValidText(AText: string): Boolean;
begin
  Result := FMask.IsFullValid(AText);
  if not Result and Properties.IgnoreMaskBlank then
    Result := AText = GetFormattedText('');
end;

procedure TcxMaskEditStandardMode.LMouseDown;
begin
  if FEdit.HandleAllocated and Properties.IsMasked then
  begin
    if (FEdit.SelLength = 0) and (FEdit.SelStart < Length(FEdit.EditText)) then
      FEdit.DirectSetSelLength(1);
  end;
end;

procedure TcxMaskEditStandardMode.PrePasteFromClipboard;
var
  AText: string;
begin
  AText := Clipboard.AsText;
  FSelStart := FEdit.SelStart;
  FEdit.DirectSetSelStart(FSelStart);
  FEdit.DirectSetSelLength(Length(AText));
end;

function TcxMaskEditStandardMode.PressBackSpace: Boolean;
begin
  Result := False;

  if FEdit.SelLength <= 1 then
    PressLeft;
  PressDelete;
end;

function TcxMaskEditStandardMode.PressDelete: Boolean;
var
  ABlank: Char;
  ASelStart: Integer;
  AText: string;
  I: Integer;
begin
  Result := False;

  FEdit.LockChangeEvents(True);
  try
    AText := FEdit.EditText;
    ASelStart := FEdit.SelStart;
    for I := FEdit.SelStart to FEdit.SelStart + FEdit.SelLength - 1 do
    begin
      if FMask.Items[I] is TcxStandardMaskManyItem then
      begin
        ABlank := GetBlank(I + 1);
        if ABlank <> #0 then
        begin
          Delete(AText, I + 1, 1);
          Insert(ABlank, AText, I + 1);
          FEdit.SetInternalDisplayValue(AText);
          FEdit.SelStart := ASelStart;
        end;
      end;
    end;
  finally
    FEdit.LockChangeEvents(False);
  end;
end;

function TcxMaskEditStandardMode.PressEnd: Boolean;
begin
  if FEdit.FShiftOn then
  begin
    Result := True;
    Exit;
  end
  else
    Result := False;

  FEdit.SelStart := Length(FEdit.EditText);
end;

function TcxMaskEditStandardMode.PressHome: Boolean;
begin
  if FEdit.FShiftOn then
  begin
    Result := True;
    Exit;
  end
  else
    Result := False;

  FEdit.SelStart := 0;
  if FMask.Count > 0 then
    if FMask.Items[FEdit.SelStart] is TcxStandardMaskLiteralItem then
      PressRight;
end;

function TcxMaskEditStandardMode.PressLeft: Boolean;
  function GetSelStart: Integer;
  var
    I: Integer;
    AReset: Boolean;
    AEnd: Integer;
  begin
    Result := 0;
    AReset := True;
    AEnd := FEdit.SelStart + FEdit.SelLength;
    if AEnd >= Length(FEdit.EditText) then
      Dec(AEnd);
    if AEnd < 0 then
      AEnd := 0;
    for I := FEdit.SelStart to AEnd do
    begin
      if FMask.Items[I] is TcxStandardMaskLiteralItem then
        Inc(Result)
      else
      begin
        AReset := False;
        Break;
      end;
    end;
    if AReset then
    begin
      if FEdit.SelStart + FEdit.SelLength >= Length(FEdit.EditText) then
      begin
        Result := AEnd - FEdit.SelStart;
        Inc(Result);
      end
      else
        Result := 0;
    end;
  end;
var
  ADec: Integer;
  ALeftLiteralCount: Integer;
  I: Integer;
begin
  if FEdit.FShiftOn then
  begin
    if FEdit.SelLength = 1 then
    begin
      FEdit.SelStart := FEdit.SelStart + 1;
      FEdit.SelLength := 0;
      FEdit.SendMyKeyDown(VK_LEFT, []);
      FEdit.SendMyKeyDown(VK_LEFT, []);
    end;

    Result := True;
    Exit;
  end
  else
    Result := False;

  if FEdit.SelLength > 1 then
  begin
    I := FEdit.SelStart + GetSelStart;
    FEdit.SelStart := 0;
    FEdit.SelStart := I;
    Exit;
  end;

  ALeftLiteralCount := 0;
  for I := FEdit.SelStart - 1 downto 0 do
  begin
    if FMask.Items[I] is TcxStandardMaskLiteralItem then
      Inc(ALeftLiteralCount)
    else
      Break;
  end;

  ADec := ALeftLiteralCount + 1;
  if FEdit.SelStart - ADec < 0 then
    ADec := 0;

  if FEdit.SelStart > 0 then
    FEdit.SelStart := FEdit.SelStart - ADec;
end;

function TcxMaskEditStandardMode.PressRight: Boolean;
  function GetSelStart(AI: Integer): Integer;
  var
    I: Integer;
    AReset: Boolean;
  begin
    Result := 0;
    AReset := True;
    for I := AI downto FEdit.SelStart do
    begin
      if FMask.Items[I] is TcxStandardMaskLiteralItem then
        Inc(Result)
      else
      begin
        AReset := False;
        Break;
      end;
    end;
    if AReset then
      Result := 0;
  end;
var
  AInc: Integer;
  ARightLiteralCount: Integer;
  I: Integer;
begin
  if FEdit.FShiftOn then
  begin
    if (FEdit.SelLength = 1) and (FEdit.SelStart = FEdit.CursorPos) then
      FEdit.SelLength := 0;

    Result := True;
    Exit;
  end
  else
    Result := False;

  if FEdit.SelLength > 1 then
  begin
    I := FEdit.SelStart + FEdit.SelLength;
    if I < Length(FEdit.EditText) then
    begin
      Dec(I);
      Dec(I, GetSelStart(I));
    end;
    FEdit.SelStart := 0;
    FEdit.SelStart := I;
    Exit;
  end;

  ARightLiteralCount := 0;
  for I := FEdit.SelStart + 1 to FMask.Count - 1 do
  begin
    if FMask.Items[I] is TcxStandardMaskLiteralItem then
      Inc(ARightLiteralCount)
    else
      Break;
  end;

  AInc := ARightLiteralCount + 1;
  if FEdit.SelStart + AInc > Length(FEdit.EditText) then
    AInc := 0;

  FEdit.SelStart := FEdit.SelStart + AInc;
end;

function TcxMaskEditStandardMode.PressSymbol(var ASymbol: Char): Boolean;
var
  AText: string;
  ASelStart: Integer;
begin
  Result := False;
  if Length(FEdit.EditText) <= 0 then
    Exit;
  if FEdit.SelStart >= Length(FEdit.EditText) then
    Exit;
  if FMask.Items[FEdit.SelStart] is TcxStandardMaskLiteralItem then
  begin
    FEdit.FShiftOn := False;
    if FEdit.SelLength > 1 then
      PressDelete;
    ASelStart := FEdit.SelStart;
    PressRight;
    if  FEdit.SelStart > ASelStart then
      PressSymbol(ASymbol);
  end
  else
  begin
    if FMask.Items[FEdit.SelStart].Check(ASymbol) and
      FEdit.IsCharValidForPos(ASymbol, FEdit.SelStart + 1) then
        if ASymbol <> #0 then
        begin
          if FEdit.SelLength > 1 then
            PressDelete;
          AText := FEdit.EditText;
          ASelStart := FEdit.SelStart;
          Delete(AText, FEdit.SelStart + 1 , 1);
          Insert(ASymbol, AText, FEdit.SelStart + 1);
          FEdit.SetInternalDisplayValue(AText);
          FEdit.SelStart := ASelStart;
          FEdit.FShiftOn := False;
          PressRight;
        end
        else
          BeepOnError;
  end;
end;

function TcxMaskEditStandardMode.ProcessSelText(const Value: string;
  out ANewText: string; out ANewSelStart: Integer): Boolean;
var
  ALength: Integer;
  I, J: Integer;
  ASymbol: Char;
begin
  ALength := Length(Value);
  Result := ALength > 0;
  if Result then
  begin
    ANewSelStart := FEdit.SelStart + ALength;
    J := FEdit.SelStart;
    ANewText := FEdit.EditText;
    for I := 1 to ALength do
    begin
      ASymbol := Value[I];
      if J = FMask.Count then
        Break;
      while (FMask.Items[J] is TcxStandardMaskLiteralItem) and not FMask.Items[J].Check(ASymbol) do
      begin
        Inc(J);
        Inc(ANewSelStart);
      end;
      ANewText[J + 1] := ASymbol;
      Inc(J);
    end;
    ANewSelStart := Max(0, Min(ANewSelStart, Length(ANewText)));
  end;
end;

procedure TcxMaskEditStandardMode.SetText(AText: string);
begin
  LMouseDown;
end;

procedure TcxMaskEditStandardMode.SynchronizeEditValue;
begin
  inherited SynchronizeEditValue;
  LMouseDown;
end;

procedure TcxMaskEditStandardMode.UpdateEditValue;
begin
  if FNeedUpdateEditValue then
  begin
    FEdit.InternalEditValue := FMask.EmptyString;
    FNeedUpdateEditValue := False;
  end;
end;

function TcxMaskEditStandardMode.GetUpdatedText(const AText: string;
  AMatchForBlanksAndLiterals: Boolean = True): string;
begin
  Result := AText;
  if FMask.Count > 0 then
  begin
    FMask.Format(Result, Properties.CharCase = ecNormal, AMatchForBlanksAndLiterals);
    FMask.Format2(Result);
  end;
end;

function TcxMaskEditStandardMode.GetBlank(APos: Integer): Char;
begin
  Result := FMask.Blank;
end;

{ TcxMaskEditRegExprMode }

constructor TcxMaskEditRegExprMode.Create(AEdit: TcxCustomMaskEdit;
    AProperties: TcxCustomMaskEditProperties);
begin
  inherited Create(AEdit, AProperties);

  FRegExpr := TcxRegExpr.Create;
  FRegExpr.CaseInsensitive := Properties.CaseInsensitive;
  FMouseAction := False;
end;

destructor TcxMaskEditRegExprMode.Destroy;
begin
  FRegExpr.Free;
  inherited Destroy;
end;

procedure TcxMaskEditRegExprMode.AfterPasteFromClipboard;
begin
end;

procedure TcxMaskEditRegExprMode.Compile(AMask: string);
var
  I: Integer;
  AStream: TStringStream;
  AStr: string;
begin
  FEditMask := AMask;
  if Properties.EmptyMask(AMask) then
    Exit;

  AStream := TStringStream.Create(AMask, TEncoding.UTF8);
  try
    try
      FRegExpr.Compile(AStream);
    except
      on E: EcxRegExprError do
      begin
        AStr := cxGetResourceString(@scxMaskEditRegExprError);
        for I := 0 to E.Errors.Count - 1 do
          AStr := AStr + #13#10 + E.Errors[I].FullMessage;
        raise EcxMaskEditError.Create(AStr);
      end;
    end;
  finally
    AStream.Free;
  end;
end;

function TcxMaskEditRegExprMode.PressDelete: Boolean;
begin
  CursorCorrection;
  if FEdit.SelLength <= 0 then
  begin
    if FTail <> '' then
    begin
      FSelect := FTail[1];
      Delete(FTail, 1, 1);

      if not NextTail then
      begin
        ClearTail;
        if FEdit.SelStart > 0 then
          FEdit.DirectSetSelStart(FEdit.SelStart - 1);
      end;
    end;

    Result := True;
  end
  else
  begin
    DeleteSelection;
    if NextTail then
      Result := True
    else
    begin
      FEdit.SendMyKeyDown(VK_DELETE, []);
      ClearTail;

      Result := False;
    end;
  end;

  FSelect := '';

  if not Result then
    BeepOnError;
end;

function TcxMaskEditRegExprMode.PressEnd: Boolean;
var
  I: Integer;
begin
  CursorCorrection;
  Result := True;

  if FTail <> '' then
  begin
    for I := 1 to Length(FTail) do
    begin
      FRegExpr.Next(FTail[I]);
    end;
    FHead := FHead + FTail;
    FTail := '';
  end;
end;

function TcxMaskEditRegExprMode.PressHome: Boolean;
begin
  CursorCorrection;
  Result := True;

  if FHead <> '' then
  begin
    FTail := FHead + FTail;
    FHead := '';
    FRegExpr.Reset;
  end;
end;

function TcxMaskEditRegExprMode.GetEmptyString: string;
begin
  Result := '';
end;

function TcxMaskEditRegExprMode.GetFormattedText(const AText: string; AMatchForBlanksAndLiterals: Boolean = True): string;
var
  I: Integer;
  ATextCopy: string;
begin
  if not FRegExpr.IsCompiled then
  begin
    Result := '';
    Exit;
  end;
  FRegExpr.Reset;
  Result := '';
  ATextCopy := AText;
  for I := 1 to Length(ATextCopy) do
  begin
    if FRegExpr.Next(ATextCopy[I]) then
      Result := Result + ATextCopy[I];
  end;
end;

procedure TcxMaskEditRegExprMode.GotoEnd;
var
  I: Integer;
begin
  CursorCorrection;
  if FTail = '' then
    Exit;

  for I := 1 to Length(FTail) do
    FRegExpr.Next(FTail[I]);

  FHead := FHead + FTail;
  FTail := '';
end;

function TcxMaskEditRegExprMode.IsCursorBegin: Boolean;
begin
  Result := FEdit.SelStart = 0;
end;

function TcxMaskEditRegExprMode.IsCursorEnd: Boolean;
begin
  Result := FEdit.SelStart = Length(FEdit.Text);
end;

function TcxMaskEditRegExprMode.IsFullValidText(AText: string): Boolean;
var
  ARegExpr: TcxRegExpr;
  I: Integer;
begin
  Result := AText = '';

  if not Result then
  begin
    ARegExpr := TcxRegExpr.Create;
    try
      ARegExpr.CaseInsensitive := Properties.CaseInsensitive;

      Result := CompileRegExpr(ARegExpr);
      if Result then
      begin
        for I := 1 to Length(AText) do
        begin
          if not ARegExpr.Next(AText[I]) then
          begin
            Result := False;
            Break;
          end;
        end;

        if Result then
          if not Properties.IgnoreMaskBlank then
            Result := ARegExpr.IsFinal;
      end;
    finally
      ARegExpr.Free;
    end;
  end;
end;

procedure TcxMaskEditRegExprMode.LMouseDown;
begin
  FMouseAction := True;
end;

procedure TcxMaskEditRegExprMode.PrePasteFromClipboard;
begin
end;

function TcxMaskEditRegExprMode.PressBackSpace: Boolean;
begin
  CursorCorrection;
  if FEdit.SelLength <= 0 then
  begin
    if FHead <> '' then
    begin
      FRegExpr.Prev;
      if NextTail then
        Delete(FHead, Length(FHead), 1)
      else
        ClearTail;
    end;

    Result := True;
  end
  else
  begin
    DeleteSelection;
    if NextTail then
      Result := True
    else
    begin
      FEdit.SendMyKeyPress(#8);

      ClearTail;

      Result := False;
    end;
  end;

  FSelect := '';

  if not Result then
    BeepOnError;
end;

function TcxMaskEditRegExprMode.PressLeft: Boolean;
var
  I: Integer;
begin
  CursorCorrection;
  Result := True;

  if FHead <> '' then
  begin
   if FEdit.SelLength > 0 then
   begin
     if (FEdit.CursorPos = FEdit.SelStart + FEdit.SelLength) and
         not FEdit.FShiftOn then
     begin
       for I := 0 to FEdit.SelLength - 1 do
       begin
         FRegExpr.Prev;
         FTail := FHead[Length(FHead)] + FTail;
         Delete(FHead, Length(FHead), 1);
       end;

       Exit;
     end
     else if (FEdit.CursorPos = FEdit.SelStart) and not FEdit.FShiftOn then
       Exit;
   end;

   FRegExpr.Prev;
   FTail := FHead[Length(FHead)] + FTail;
   Delete(FHead, Length(FHead), 1);
  end;
end;

function TcxMaskEditRegExprMode.PressRight: Boolean;

  procedure GetTailFirstChar;
  begin
    FRegExpr.Next(FTail[1]);
    FHead := FHead + FTail[1];
    Delete(FTail, 1, 1);
  end;

var
  I: Integer;
begin
  CursorCorrection;
  Result := True;

  if FTail <> '' then
  begin
    if FEdit.SelLength > 0 then
    begin
      if (FEdit.CursorPos = FEdit.SelStart) and
          not FEdit.FShiftOn then
      begin
        for I := 0 to FEdit.SelLength - 1 do
          GetTailFirstChar;
        Exit;
      end
      else if (FEdit.CursorPos = FEdit.SelStart + FEdit.SelLength) and
          not FEdit.FShiftOn then
        Exit;
    end;

    GetTailFirstChar;
  end;
end;

function TcxMaskEditRegExprMode.PressSymbol(var ASymbol: Char): Boolean;
begin
  CursorCorrection;
  if FEdit.SelLength > 0 then
    DeleteSelection;

  if FRegExpr.Next(ASymbol) then
  begin
    FHead := FHead + ASymbol;
    if not NextTail then
    begin
      if FSelect <> '' then
      begin
        FEdit.SendMyKeyDown(VK_DELETE, []);
        FEdit.SendMyKeyPress(ASymbol);
        Result := False;
      end
      else
        Result := True;

      ClearTail;
    end
    else
      Result := True;
  end
  else
  begin
    RestoreSelection;
    Result := False;
  end;

  FSelect := '';

  if not Result then
    BeepOnError;
end;

procedure TcxMaskEditRegExprMode.SetText(AText: string);
var
  I: Integer;
begin
  if (ClipboardTextLength > 0) and (Length(AText) > 0) then
  begin
    FRegExpr.Reset;
    for I := 1 to FEdit.SelStart + ClipboardTextLength do
      FRegExpr.Next(AText[I]);

    FHead := Copy(AText, 1, FEdit.SelStart + ClipboardTextLength);
    FTail := Copy(AText, FEdit.SelStart + ClipboardTextLength + 1, Length(AText));

    ClipboardTextLength := 0;
  end
  else
    Reset(AText);
end;

procedure TcxMaskEditRegExprMode.SetRegExprCaseInsensitive;
begin
  FRegExpr.CaseInsensitive := Properties.CaseInsensitive;
end;

procedure TcxMaskEditRegExprMode.UpdateEditValue;
begin
end;

function TcxMaskEditRegExprMode.GetUpdatedText(const AText: string;
  AMatchForBlanksAndLiterals: Boolean = True): string;
begin
  Result := AText;
end;

procedure TcxMaskEditRegExprMode.ClearTail;
var
  AStr: string;
begin
  AStr := FEdit.DataBinding.DisplayValue;
  Delete(AStr, FEdit.SelStart + 1, Length(FTail));
  FEdit.DataBinding.DisplayValue := AStr;
  FEdit.DirectSetSelStart(Length(AStr));
  FTail := '';
end;

function TcxMaskEditRegExprMode.CompileRegExpr(
  ARegExpr: TcxRegExpr): Boolean;
begin
  if FRegExpr.Stream = nil then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  try
    ARegExpr.Compile(FRegExpr.Stream);
  except
    on EcxMaskEditError do
      Result := False;
  end;
end;

function TcxMaskEditRegExprMode.NeedReset: Boolean;
begin
  Result := FMouseAction or
    (FEdit.Text <> '') and (FHead = '') and
    ((FHead + FTail <> FEdit.Text) or (FEdit.CursorPos <> Length(FHead)));
end;

procedure TcxMaskEditRegExprMode.CursorCorrection;
var
  I: Integer;
  ASymbol: Char;
begin
  if NeedReset then
  begin
    FMouseAction := False;
    Reset(FEdit.Text);
    for I := 0 to FEdit.CursorPos - 1 do
    begin
      if Length(FEdit.Text) > I then
      begin
        ASymbol := Char(FEdit.Text[I + 1]);
        FRegExpr.Next(ASymbol);
        FHead := FHead + FTail[1];
        Delete(FTail, 1, 1);
      end;
    end;
  end;
end;

procedure TcxMaskEditRegExprMode.DeleteSelection;
var
  I: Integer;
begin
  if FEdit.SelStart = Length(FHead) then  // Begin cursor
  begin
    FSelect := Copy(FTail, 1, FEdit.SelLength);
    Delete(FTail, 1, FEdit.SelLength);

    FBeginCursor := True;
  end
  else
    if (FEdit.SelStart + FEdit.SelLength) = Length(FHead) then // End cursor
    begin
      FSelect := Copy(FHead, FEdit.SelStart + 1, FEdit.SelLength);
      Delete(FHead, FEdit.SelStart + 1, FEdit.SelLength);
      for I := 1 to Length(FSelect) do
        FRegExpr.Prev;

      FBeginCursor := False;
    end;
end;

function TcxMaskEditRegExprMode.GetMaskKind: TcxEditMaskKind;
begin
  Result := emkRegExpr;
end;

function TcxMaskEditRegExprMode.NextTail: Boolean;
var
  AIsCharValid: Boolean;
  I, J, NextNumber: Integer;
begin
  NextNumber := 0;

  for I := 1 to Length(FTail) do
  begin
    AIsCharValid := FRegExpr.Next(FTail[I]);
    if AIsCharValid then
      Inc(NextNumber)
    else
    begin
      for J := 0 to NextNumber - 1 do
        FRegExpr.Prev;

      Result := False;
      Exit;
    end;
  end;

  for I := 1 to Length(FTail) do
    FRegExpr.Prev;

  Result := True;
end;

procedure TcxMaskEditRegExprMode.RestoreSelection;
var
  I: Integer;
begin
  if FBeginCursor then
    FTail := FSelect + FTail
  else
  begin
    FHead := FHead + FSelect;
    for I := 1 to Length(FSelect) do
      FRegExpr.Next(FSelect[I]);
  end;
end;

procedure TcxMaskEditRegExprMode.Reset(const AText: string);
begin
  FHead := '';
  FTail := AText;
  FRegExpr.Reset;
end;

procedure TcxMaskEditRegExprMode.SynchronizeEditValue;
begin
  inherited SynchronizeEditValue;
  FEdit.SelStart := Length(FEdit.Text);
  Reset(FEdit.Text);
  FRegExpr.NextEx(FHead);
end;

{ TcxMaskEditRegExprExMode }

constructor TcxMaskEditRegExprExMode.Create(AEdit: TcxCustomMaskEdit;
    AProperties: TcxCustomMaskEditProperties);
begin
  inherited Create(AEdit, AProperties);

  FRegExpr.OnSymbolUpdate := SymbolUpdate;
  FRegExpr.OnSymbolDelete := SymbolDelete;
  FRegExpr.UpdateOn := True;
  FNewCursorPos := -1;
  Clear;
end;

destructor TcxMaskEditRegExprExMode.Destroy;
begin
  inherited Destroy;
end;

procedure TcxMaskEditRegExprExMode.Clear;
begin
  FUpdate := '';
  FDeleteNumber := 0;
end;

procedure TcxMaskEditRegExprExMode.RestoreSelection;
begin
  FRegExpr.UpdateOn := False;

  inherited RestoreSelection;

  FRegExpr.UpdateOn := True;

  if FUpdate <> '' then
  begin
    FRegExpr.Prev;
    Clear;
  end;
end;

procedure TcxMaskEditRegExprExMode.SymbolDelete;
begin
  Inc(FDeleteNumber);
end;

procedure TcxMaskEditRegExprExMode.SymbolUpdate(ASymbol: Char);
begin
  FUpdate := FUpdate + ASymbol;
end;

procedure TcxMaskEditRegExprExMode.AfterPasteFromClipboard;
begin
  if FNewCursorPos < 0 then
    Exit;

  FEdit.DirectSetSelStart(FNewCursorPos);
  FNewCursorPos := -1;
end;

procedure TcxMaskEditRegExprExMode.Compile(AMask: string);
var
  I: Integer;
  AStream: TStringStream;
  AStr: string;
begin
  Clear;
  FEditMask := AMask;
  if Properties.EmptyMask(AMask) then
    Exit;

  AStream := TStringStream.Create(AMask, TEncoding.UTF8);
  try
    try
      FRegExpr.Compile(AStream);
    except
      on E: EcxRegExprError do
      begin
        AStr := cxGetResourceString(@scxMaskEditRegExprError);
        for I := 0 to E.Errors.Count - 1 do
          AStr := AStr + #13#10 + E.Errors[I].FullMessage;
        raise EcxMaskEditError.Create(AStr);
      end;
    end;
  finally
    AStream.Free;
  end;

  FNeedUpdateEditValue := not HasEdit;
end;

function TcxMaskEditRegExprExMode.GetEmptyString: string;
var
  ARegExpr: TcxRegExpr;
begin
  ARegExpr := TcxRegExpr.Create;
  try
    ARegExpr.CaseInsensitive := Properties.CaseInsensitive;
    ARegExpr.UpdateOn := False;

    if CompileRegExpr(ARegExpr) then
    begin
      ARegExpr.OnSymbolUpdate := InternalSymbolUpdate;
      FInternalUpdate := '';
      ARegExpr.UpdateOn := True;
      Result := FInternalUpdate;
    end
    else
      Result := '';
  finally
    ARegExpr.Free;
  end;
end;

function TcxMaskEditRegExprExMode.GetFormattedText(const AText: string; AMatchForBlanksAndLiterals: Boolean = True): string;
begin
  if not FRegExpr.IsCompiled then
  begin
    Result := '';
    Exit;
  end;

  FRegExpr.UpdateOn := False;
  Clear;

  Result := inherited GetFormattedText(AText, AMatchForBlanksAndLiterals);

  FRegExpr.UpdateOn := True;
  Result := Result + FUpdate;
end;

procedure TcxMaskEditRegExprExMode.GotoEnd;
begin
  FRegExpr.UpdateOn := False;

  inherited GotoEnd;

  FRegExpr.UpdateOn := True;
end;

function TcxMaskEditRegExprExMode.IsFullValidText(AText: string): Boolean;
var
  ARegExpr: TcxRegExpr;

  function IsStart: Boolean;
  begin
    ARegExpr.UpdateOn := True;
    Result := AText = FInternalUpdate;
  end;

var
  I: Integer;
begin
  ARegExpr := TcxRegExpr.Create;
  try
    ARegExpr.CaseInsensitive := Properties.CaseInsensitive;
    ARegExpr.UpdateOn := False;
    Result := CompileRegExpr(ARegExpr);

    if Result then
    begin
      ARegExpr.OnSymbolUpdate := InternalSymbolUpdate;
      FInternalUpdate := '';
      if not IsStart then
      begin
        ARegExpr.UpdateOn := False;
        ARegExpr.Reset;
        for I := 1 to Length(AText) do
        begin
          if not ARegExpr.Next(AText[I]) then
          begin
            Result := False;
            Break;
          end;
        end;

        if Result then
          if not Properties.IgnoreMaskBlank then
            Result := ARegExpr.IsFinal;
      end;
    end;
  finally
    ARegExpr.Free;
  end;
end;

procedure TcxMaskEditRegExprExMode.PrePasteFromClipboard;
begin
  CursorCorrection;
end;

function TcxMaskEditRegExprExMode.PressBackSpace: Boolean;
var
  ASelLength: Integer;
  I: Integer;
  AText: string;
  ADeletedCharCount: integer;
begin
  CursorCorrection;
  Clear;

  if FEdit.SelLength <= 0 then
  begin
    if FHead = '' then
    begin
      Result := False;
      Exit;
    end;

    FRegExpr.Prev;

    if FRegExpr.IsStart then
      if FEdit.SelStart = FDeleteNumber then
      begin
        FRegExpr.Next(FHead[1]);
        Result := False;
        BeepOnError;
        Exit;
      end;

    AText := FEdit.Text;
    for I := 0 to FDeleteNumber do
      FEdit.SendMyKeyPress(#8);
    ADeletedCharCount := Length(AText) - Length(FEdit.Text);
    Delete(FHead, Length(FHead) - ADeletedCharCount + 1, ADeletedCharCount);

    FRegExpr.UpdateOn := False;

    if NextTail then
      UpdateTail
    else
      ClearTail;

    FRegExpr.UpdateOn := True;
  end
  else
  begin
    DeleteSelection;

    if FEdit.SelStart = 0 then
    begin
      FRegExpr.UpdateOn := False;
      FRegExpr.UpdateOn := True;
      if FUpdate <> '' then
      begin
        FHead := FUpdate;
        Clear;
        ASelLength := FEdit.SelLength;
        FEdit.SelStart := Length(FHead);
        FEdit.SelLength := ASelLength - FEdit.SelStart;
        FTail := Copy(FEdit.Text, FEdit.SelStart + 1, FEdit.SelLength) + FTail;
        Result := PressBackSpace;
        Exit;
      end
    end;

    FEdit.SendMyKeyPress(#8);

    FRegExpr.UpdateOn := False;

    if NextTail then
      UpdateTail
    else
      ClearTail;

    FRegExpr.UpdateOn := True;
  end;

  Result := False;
end;

function TcxMaskEditRegExprExMode.PressDelete: Boolean;
var
  I: Integer;
begin
  CursorCorrection;
  Clear;

  if FEdit.SelLength <= 0 then
  begin
    if FTail = '' then
    begin
      Result := False;
      Exit;
    end;

    if FEdit.SelStart = 0 then
    begin
      FRegExpr.UpdateOn := False;
      FRegExpr.UpdateOn := True;
      if FUpdate <> '' then
      begin
        FRegExpr.Prev;
        Clear;
        Result := False;
        BeepOnError;
        Exit;
      end;
    end;

    FRegExpr.Next(FTail[1]);
    for I := 0 to Length(FUpdate) do
      FEdit.SendMyKeyDown(VK_DELETE, []);
    Delete(FTail, 1, Length(FUpdate) + 1);
    FRegExpr.Prev;

    FRegExpr.UpdateOn := False;

    if NextTail then
      UpdateTail
    else
      ClearTail;

    FRegExpr.UpdateOn := True;
  end
  else
    PressBackSpace;

  Result := False;
end;

function TcxMaskEditRegExprExMode.PressEnd: Boolean;
begin
  Result := True;

  CursorCorrection;
  Clear;

  FRegExpr.UpdateOn := False;

  inherited PressEnd;

  FRegExpr.UpdateOn := True;
end;

function TcxMaskEditRegExprExMode.PressHome: Boolean;
begin
  Result := True;

  CursorCorrection;
  Clear;

  inherited PressHome;
end;

function TcxMaskEditRegExprExMode.PressLeft: Boolean;
var
  I: Integer;
begin
  Result := True;

  CursorCorrection;
  Clear;

  if FEdit.SelLength > 0 then
  begin
    if (FEdit.CursorPos = FEdit.SelStart + FEdit.SelLength) and
        not FEdit.FShiftOn then
    begin
      FRegExpr.UpdateOn := False;
      inherited PressLeft;
      Clear;
      FRegExpr.UpdateOn := True;
      if FUpdate <> '' then
        FRegexpr.Prev;

      Exit;
    end
    else if (FEdit.CursorPos = FEdit.SelStart) and not FEdit.FShiftOn then
      Exit;
  end;

  inherited PressLeft;

  if FRegExpr.IsStart then
    if FEdit.SelStart = 0 then
    begin
      if FEdit.SelLength = FDeleteNumber then
        Dec(FDeleteNumber);
    end
    else
      if FEdit.SelStart = FDeleteNumber then
        Dec(FDeleteNumber);

  if FDeleteNumber > 0 then
  begin
    for I := 0 to FDeleteNumber - 1 do
    begin
      FTail := FHead[Length(FHead) - I] + FTail;
      FEdit.SendMyKeyDown(VK_LEFT, []);
    end;
    Delete(FHead, Length(FHead) - FDeleteNumber + 1, FDeleteNumber);
  end;
end;

function TcxMaskEditRegExprExMode.PressRight: Boolean;
var
  I: Integer;
begin
  Result := True;

  CursorCorrection;
  Clear;

  if FEdit.SelLength > 0 then
  begin
    if (FEdit.CursorPos = FEdit.SelStart) and
        not FEdit.FShiftOn then
    begin
      FRegExpr.UpdateOn := False;
      inherited PressRight;
      Clear;
      FRegExpr.UpdateOn := True;

      Exit;
    end
    else if (FEdit.CursorPos = FEdit.SelStart + Fedit.SelLength) and
        not FEdit.FShiftOn then
      Exit;
  end;

  inherited PressRight;

  if FUpdate <> '' then
  begin
    for I := 1 to Length(FUpdate) do
    begin
      FHead := FHead + FTail[I];
      FEdit.SendMyKeyDown(VK_RIGHT, []);
    end;
    Delete(FTail, 1, Length(FUpdate));
  end;
end;

function TcxMaskEditRegExprExMode.PressSymbol(var ASymbol: Char): Boolean;
var
  I: Integer;
  ASelLength: Integer;
begin
  CursorCorrection;
  Clear;

  if FEdit.SelLength > 0 then
  begin
    DeleteSelection;
    if FEdit.SelStart = 0 then
    begin
      FRegExpr.UpdateOn := False;
      FRegExpr.UpdateOn := True;
      if FUpdate <> '' then
      begin
        FHead := FUpdate;
        Clear;
        ASelLength := FEdit.SelLength;
        FEdit.SelStart := Length(FHead);
        FEdit.SelLength := ASelLength - FEdit.SelStart;
        FTail := Copy(FEdit.Text, FEdit.SelStart + 1, FEdit.SelLength) + FTail;
        Result := PressSymbol(ASymbol);
        Exit;
      end
    end;
  end;

  if FRegExpr.Next(ASymbol) then
  begin
    FHead := FHead + ASymbol + FUpdate;

    FEdit.SendMyKeyPress(ASymbol);
    for I := 1 to Length(FUpdate) do
      FEdit.SendMyKeyPress(FUpdate[I]);

    FRegExpr.UpdateOn := False;

    if NextTail then
      UpdateTail
    else
      ClearTail;

    FRegExpr.UpdateOn := True;
  end
  else
  begin
    if FEdit.SelLength > 0 then
      RestoreSelection;

    BeepOnError;
  end;

  FSelect := '';
  Result := False;
end;

procedure TcxMaskEditRegExprExMode.SetText(AText: string);
var
  I: Integer;
begin
  FRegExpr.UpdateOn := False;
  FRegExpr.Reset;

  for I := 1 to Length(AText) do
    FRegExpr.Next(AText[I]);

  Clear;
  FRegExpr.UpdateOn := True;
  FHead := AText + FUpdate;
  FTail := '';

  if HasEdit then
  begin
    FMouseAction := True;
    CursorCorrection;
  end;

  ClipboardTextLength := 0;
end;

procedure TcxMaskEditRegExprExMode.UpdateEditValue;
begin
  if FNeedUpdateEditValue then
  begin
    FEdit.InternalEditValue := FUpdate;
    FNeedUpdateEditValue := False;
  end;
end;

procedure TcxMaskEditRegExprExMode.CursorCorrection;

  procedure Next;
  begin
    if FTail <> '' then
    begin
      Clear;
      FRegExpr.Next(FTail[1]);
      FHead := FHead + Copy(FTail, 1, Length(FUpdate) + 1);
      Delete(FTail, 1, Length(FUpdate) + 1);
    end;
  end;

  procedure Prev;
  begin
    if FHead <> '' then
    begin
      Clear;
      FRegExpr.Prev;

      FTail := Copy(FHead, Length(FHead) - FDeleteNumber, FDeleteNumber + 1) + FTail;

      if FRegExpr.IsStart then
        FHead := ''
      else
        Delete(FHead, Length(FHead) - FDeleteNumber, FDeleteNumber + 1);
    end;
  end;

  procedure CorrectSelLength(ASelEnd: Integer);
  begin
    while True do
    begin
      Next;
      if ASelEnd <= Length(FHead) then
      begin
        FEdit.DirectSetSelLength(Length(FHead) - FEdit.SelStart);
        Break;
      end;
    end;
  end;

var
  ASelStart: Integer;
  ASelEnd: Integer;
begin
  if not HasEdit or not FEdit.HandleAllocated then
    Exit;

  if NeedReset then
  begin
    Reset(FEdit.Text);
    FMouseAction := True;
  end;

  if not FMouseAction then
    Exit
  else
    FMouseAction := False;

  ASelStart := FEdit.SelStart;
  ASelEnd := FEdit.SelStart + FEdit.SelLength;

  // Correct FEdit.SelStart
  if ASelStart > Length(FHead) then
    while True do
    begin
      Next;
      if ASelStart < Length(FHead) then
      begin
        Prev;
        FEdit.DirectSetSelStart(Length(FHead));
        Break;
      end
      else
        if ASelStart = Length(FHead) then
          Break;
    end
  else
    if ASelStart < Length(FHead) then
      while True do
      begin
        Prev;
        if ASelStart > Length(FHead) then
        begin
          FEdit.DirectSetSelStart(Length(FHead));
          Break;
        end
        else
          if ASelStart = Length(FHead) then
            Break;
      end;

  // Correct FEdit.SelLength
  if ASelEnd > ASelStart then
    CorrectSelLength(ASelEnd);
end;

procedure TcxMaskEditRegExprExMode.DeleteSelection;
begin
  FRegExpr.UpdateOn := False;

  inherited DeleteSelection;

  FRegExpr.UpdateOn := True;

  if FUpdate <> '' then
  begin
    FRegExpr.Prev;
    Clear;
  end;
end;

function TcxMaskEditRegExprExMode.GetMaskKind: TcxEditMaskKind;
begin
  Result := emkRegExprEx;
end;

procedure TcxMaskEditRegExprExMode.UpdateTail;
var
  I: Integer;
begin
  Clear;

  if FTail = '' then
    Exit;

  for I := 1 to Length(FTail) do
  begin
    FRegExpr.Next(FTail[I]);
    FEdit.DirectSetSelStart(FEdit.SelStart + 1);
  end;

  FRegExpr.UpdateOn := True;

  if FUpdate <> '' then
    for I := 1 to Length(FUpdate) do
    begin
      FTail := FTail + FUpdate[I];
      FEdit.SendMyKeyPress(FUpdate[I]);
    end;

  FRegExpr.UpdateOn := False;

  for I := 1 to Length(FTail) do
  begin
    FRegExpr.Prev;
    FEdit.DirectSetSelStart(FEdit.SelStart - 1);
  end;
end;

procedure TcxMaskEditRegExprExMode.InternalSymbolUpdate(ASymbol: Char);
begin
  FInternalUpdate := FInternalUpdate + ASymbol;
end;

{ TcxCustomMaskEditProperties }

constructor TcxCustomMaskEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);

  MaskKind := emkStandard;
  FAlwaysShowBlanksAndLiterals := False;
  ValidateOnEnter := True;
  CaseInsensitive := True;
  FTempMode := nil;
end;

destructor TcxCustomMaskEditProperties.Destroy;
begin
  FTempMode.Free;

  inherited Destroy;
end;

class function TcxCustomMaskEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxMaskEdit;
end;

function TcxCustomMaskEditProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := inherited GetSupportedOperations;
  if EditMask <> '' then
    Exclude(Result, esoEditingAutoHeight);
end;

function TcxCustomMaskEditProperties.UseLookupData: Boolean;
begin
  Result := not IsMasked and inherited UseLookupData;
end;

{function TcxCustomMaskEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  if IsMasked then
    Result := evsValue
  else
    Result := inherited GetEditValueSource(AEditFocused);
end;}

function TcxCustomMaskEditProperties.IsEditValueValid(var EditValue: TcxEditValue;
  AEditFocused: Boolean): Boolean;
begin
  Result := IsMasked or inherited IsEditValueValid(EditValue, AEditFocused);
end;

procedure TcxCustomMaskEditProperties.DoPrepareDisplayValue(
  const AEditValue: TcxEditValue; var ADisplayValue: TcxEditValue; AEditFocused: Boolean);
var
  AText: string;
begin
  if IsMasked and not (not AEditFocused and (IDefaultValuesProvider <> nil) and
    IDefaultValuesProvider.IsDisplayFormatDefined(True)) then
  begin
    AText := VarToStr(AEditValue);
    if (FTempMode = nil) or not TestTempMaskKind or not TestTempEditMask then
    begin
      FreeAndNil(FTempMode);
      FTempMode := CreateMode;
    end;
    if FTempMode <> nil then
    begin
      if not AEditFocused and (FTempMode is TcxMaskEditStandardMode) then
      begin
        if not FAlwaysShowBlanksAndLiterals then
          ADisplayValue := FTempMode.GetUpdatedText(AText, False)
        else
          ADisplayValue := FTempMode.GetFormattedText(AText, False);
      end
      else
        ADisplayValue := FTempMode.GetFormattedText(AText, False);
    end;
  end
  else
    inherited;
end;

procedure TcxCustomMaskEditProperties.ValidateDisplayValue(var ADisplayValue: TcxEditValue;
  var AErrorText: TCaption; var AError: Boolean; AEdit: TcxCustomEdit);
begin
  if not AError then
  begin
    if not IsMasked{ or IgnoreMaskBlank }then
      AError := False
    else
    begin
      AError := not TcxCustomMaskEdit(AEdit).Mode.IsFullValidText(VarToStr(ADisplayValue));
      if AError then
        AErrorText := cxGetResourceString(@scxMaskEditInvalidEditValue);
    end;
  end;

  inherited;
end;

function TcxCustomMaskEditProperties.CreateMode: TcxMaskEditCustomMode;
begin
  Result := nil;
  if not EmptyMask(EditMask) then
  begin
    Result := GetModeClass(FMaskKind).Create(nil, Self);
    Result.Compile(EditMask);
  end;
end;

function TcxCustomMaskEditProperties.CreateMode1: TcxMaskEditCustomMode;
begin
  Result := GetModeClass(MaskKind).Create(nil, Self);
end;

function TcxCustomMaskEditProperties.EmptyMask(AMask: string): Boolean;
var
  I: Integer;
begin
  Result := AMask = '';

  if not Result then
  begin
    for I := 1 to Length(AMask) do
      if not SpaceToken(AMask[I]) then
      begin
        Result := False;
        Exit;
      end;

    Result := True;
  end;
end;

procedure TcxCustomMaskEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomMaskEditProperties then
    with TcxCustomMaskEditProperties(AProperties) do
    begin
      Self.MaskKind := MaskKind;
      Self.EditMask := EditMask;
      Self.CaseInsensitive := CaseInsensitive;
      Self.IgnoreMaskBlank := IgnoreMaskBlank;
      Self.AlwaysShowBlanksAndLiterals := AlwaysShowBlanksAndLiterals;
    end
end;

procedure TcxCustomMaskEditProperties.DoChanged;
begin
  FreeAndNil(FTempMode);
  inherited;
end;

function TcxCustomMaskEditProperties.GetEditMask: TcxEditMask;
begin
  if FMaskKind <> emkStandard then
    Result := FEditMask
  else
  begin
    if EmptyMask(FEditMask) then
    begin
      if IDefaultValuesProvider <> nil then
        Result := IDefaultValuesProvider.DefaultEditMask
      else
        Result := '';
    end
    else
      Result := FEditMask
  end;
end;

function TcxCustomMaskEditProperties.GetIsMasked: Boolean;
begin
  Result := not EmptyMask(EditMask);
end;

procedure TcxCustomMaskEditProperties.SetAlwaysShowBlanksAndLiterals(AValue: Boolean);
begin
  if AValue <> FAlwaysShowBlanksAndLiterals then
  begin
    FAlwaysShowBlanksAndLiterals := AValue;
    Changed;
  end;
end;

procedure TcxCustomMaskEditProperties.SetCaseInsensitive(AValue: Boolean);
begin
  if CharCase = ecNormal then
    FCaseInsensitive := AValue
  else
    FCaseInsensitive := True;
  Changed;
end;

procedure TcxCustomMaskEditProperties.SetEditMask(Value: TcxEditMask);
var
  AMode: TcxMaskEditCustomMode;
begin
  if EditMask = Value then
    FEditMask := Value
  else
  begin
    AMode := CreateMode1;
    try
      AMode.Compile(Value);
      FEditMask := Value;
      Changed;
    finally
      AMode.Free;
    end;
  end;
end;

procedure TcxCustomMaskEditProperties.SetMaskKind(Value: TcxEditMaskKind);
begin
  if Value <> FMaskKind then
  begin
    FMaskKind := Value;
    Changed;
  end;
end;

function TcxCustomMaskEditProperties.GetDisplayFormatOptions: TcxEditDisplayFormatOptions;
begin
 if not IsMasked then
    Result := [dfoSupports]
  else
    Result := [];
end;

function TcxCustomMaskEditProperties.GetModeClass(
  AMaskKind: TcxEditMaskKind): TcxMaskEditCustomModeClass;
begin
  case AMaskKind of
    emkStandard:
      Result := TcxMaskEditStandardMode;
    emkRegExpr:
      Result := TcxMaskEditRegExprMode;
    else
      Result := TcxMaskEditRegExprExMode;
  end;
end;

procedure TcxCustomMaskEditProperties.MaxLengthChanged;
begin
  if IsMasked then
  begin
    if FMaxLengthSetting then
      Exit;
    FMaxLengthSetting := True;
    BeginUpdate;
    try
      MaxLength := 0;
    finally
      FMaxLengthSetting := False;
      EndUpdate;
    end;
  end
  else
    inherited MaxLengthChanged;
end;

function TcxCustomMaskEditProperties.SpaceToken(AToken: Char): Boolean;
begin
  Result := IsSpaceChar(AToken);
end;

function TcxCustomMaskEditProperties.GetEmptyString: string;
begin
  Result := FEmptyString;
end;

procedure TcxCustomMaskEditProperties.SetCharCase(Value: TEditCharCase);
begin
  if Value in [ecLowerCase, ecUpperCase] then
    FCaseInsensitive := True;

  inherited SetCharCase(Value);
end;

function TcxCustomMaskEditProperties.TestTempEditMask: Boolean;
begin
  Result := FTempMode.EditMask = EditMask;
end;

function TcxCustomMaskEditProperties.TestTempMaskKind: Boolean;
begin
  Result := False;
  case FMaskKind of
    emkStandard:
      Result := FTempMode is TcxMaskEditStandardMode;
    emkRegExpr:
      Result := FTempMode is TcxMaskEditRegExprMode;
    emkRegExprEx:
      Result := FTempMode is TcxMaskEditRegExprExMode;
  end;
end;

{ TcxCustomMaskEdit }

constructor TcxCustomMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TcxCustomMaskEdit.Destroy;
begin
  FreeAndNil(FMode);
  inherited Destroy;
end;

procedure TcxCustomMaskEdit.CutToClipboard;
begin
  if ActiveProperties.IsMasked then
  begin
    if not Focused or DoEditing then
    begin
      CopyToClipboard;
      if Mode.PressDelete then
        SendMyKeyPress(Char(VK_BACK));
    end
  end
  else
    inherited CutToClipBoard;
end;

class function TcxCustomMaskEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomMaskEditProperties;
end;

procedure TcxCustomMaskEdit.SendMyKeyDown(Key: Word; Shift: TShiftState);
begin
  FMyMessage := True;
  try
    SendKeyDown(Self, Key, Shift);
  finally
    FMyMessage := False;
  end;
end;

procedure TcxCustomMaskEdit.SendMyKeyPress(AKey: Char);
begin
  FMyMessage := True;
  try
    SendKeyPress(Self, AKey);
  finally
    FMyMessage := False;
  end;
end;

procedure TcxCustomMaskEdit.ChangeHandler(Sender: TObject);
begin
  FText := FMode.GetUpdatedText(InnerEdit.EditValue);

  inherited ChangeHandler(Sender);
end;

procedure TcxCustomMaskEdit.DblClick;
begin
  if ActiveProperties.IsMasked then
  begin
    Mode.GotoEnd;
    Mode.LMouseDown;
  end;

  inherited DblClick;
end;

procedure TcxCustomMaskEdit.DoEnter;
begin
  if ActiveProperties.IsMasked then
    if ActiveProperties.AutoSelect then
      Mode.GotoEnd;

  inherited DoEnter;
end;

procedure TcxCustomMaskEdit.DoEditKeyDown(var Key: Word; Shift: TShiftState);
var
  AAfterKeyDownNeed: Boolean;
begin
  if not ActiveProperties.IsMasked or IsSpecialKey(Key, Shift) then
  begin
    inherited DoEditKeyDown(Key, Shift);
    Exit;
  end;

  if not ValidateKeyDown(Key, Shift) then
  begin
    DoAfterKeyDown(Key, Shift);
    Key := 0;
    Exit;
  end;

  if Key = VK_TAB then
  begin
    inherited DoEditKeyDown(Key, Shift);
    Exit;
  end;

  FShiftOn := ssShift in Shift;

  if ((Key = VK_UP) or (Key = VK_DOWN)) and not HasPopupWindow then
  begin
    DoAfterKeyDown(Key, Shift);
    Key := 0;
  end
  else if Key = VK_LEFT then
  begin
    if ssCtrl in Shift then
    begin
      SendMyKeyDown(VK_HOME, Shift);
      if not Mode.PressHome then
        Key := 0;
    end
    else
    begin
      AAfterKeyDownNeed := IsCursorBegin;
      if not Mode.PressLeft then
      begin
        if AAfterKeyDownNeed then
          DoAfterKeyDown(Key, Shift);
        Key := 0;
      end;
    end;
  end
  else if Key = VK_RIGHT then
  begin
    if ssCtrl in Shift then
    begin
      SendMyKeyDown(VK_END, Shift);
      if not Mode.PressEnd then
        Key := 0;
    end
    else
    begin
      AAfterKeyDownNeed := IsCursorEnd;
      if not Mode.PressRight then
      begin
        if AAfterKeyDownNeed then
          DoAfterKeyDown(Key, Shift);
        Key := 0;
      end;
    end;
  end
  else if Key = VK_DELETE then
  begin
    if ssShift in Shift then
    begin
      if SelLength = 0 then
        if not Mode.PressBackSpace then
          Key := 0;
    end
    else
      if not Mode.PressDelete then
        Key := 0;
  end
  else if Key = VK_HOME then
  begin
    if not Mode.PressHome then
      Key := 0;
  end
  else if Key = VK_END then
  begin
    if not Mode.PressEnd then
      Key := 0;
  end;

  if Key <> 0 then
    inherited  DoEditKeyDown(Key, Shift);
end;

procedure TcxCustomMaskEdit.DoEditKeyPress(var Key: Char);
begin
  if not ActiveProperties.IsMasked then
  begin
    inherited DoEditKeyPress(Key);
    Exit;
  end;

  if (Key = #9) or (Key = #27) then
    Key := #0
  else if not ValidateKeyPress(Key) then
    Key := #0
  else
  begin
    if Key <> #13 then
    begin
      if not ActiveProperties.IsMasked then
        inherited DoEditKeyPress(Key)
      else
      begin
        if dxCharInSet(Key, [#3, #22, #24{$IFDEF DELPHI16}, #1{$ENDIF}]) then // ^c ^v ^x ^a
        begin
          inherited DoEditKeyPress(Key);
        end
        else
        begin
          if Key = #8 then  // Backspace
          begin
            if not Mode.PressBackSpace then
              Key := #0;
          end
          else
            if not Mode.PressSymbol(Key) then
              Key := #0;
        end;
      end;
    end;
  end;
end;

function TcxCustomMaskEdit.GetEditText: TCaption;
begin
  Result := InnerEdit.EditValue;
end;

function TcxCustomMaskEdit.GetProperties: TcxCustomMaskEditProperties;
begin
  Result := TcxCustomMaskEditProperties(inherited Properties);
end;

function TcxCustomMaskEdit.GetActiveProperties: TcxCustomMaskEditProperties;
begin
  Result := TcxCustomMaskEditProperties(InternalGetActiveProperties);
end;

function TcxCustomMaskEdit.InternalGetEmptyString: string;
begin
  if ActiveProperties.IsMasked then
  begin
    if Mode <> nil then
      Result := Mode.GetEmptyString
    else
      Result := '';
  end
  else
    Result := '';
end;

procedure TcxCustomMaskEdit.InternalSetValue(AValue: string);
begin
  if ActiveProperties.IsMasked and CanSynchronizeModeText then
    Mode.SetText(AValue);
end;

function TcxCustomMaskEdit.IsCursorBegin: Boolean;
begin
  Result := FMode.IsCursorBegin;
end;

function TcxCustomMaskEdit.IsCursorEnd: Boolean;
begin
  Result := FMode.IsCursorEnd;
end;

procedure TcxCustomMaskEdit.SetEditText(AValue: TCaption);
var
  AEditText: string;
begin
  if not Focused and (Mode is TcxMaskEditStandardMode) then
  begin
    if not ActiveProperties.AlwaysShowBlanksAndLiterals then
      AEditText := Mode.GetUpdatedText(AValue)
    else
      AEditText := Mode.GetFormattedText(AValue);
  end
  else
    AEditText := Mode.GetFormattedText(AValue);
  InnerEdit.EditValue := AEditText;
  FEditValue := FText;
end;

procedure TcxCustomMaskEdit.SetProperties(Value: TcxCustomMaskEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomMaskEdit.Initialize;
begin
  inherited Initialize;
  FMode := ActiveProperties.GetModeClass(emkStandard).Create(Self, ActiveProperties);
  FShiftOn := False;
  ActiveProperties.FEmptyString := InternalGetEmptyString;
end;

function TcxCustomMaskEdit.InternalGetText: string;
begin
  if ActiveProperties.IsMasked then
    Result := FText
  else
    Result := inherited InternalGetText;
end;

procedure TcxCustomMaskEdit.InternalSetDisplayValue(const Value: TcxEditValue);
var
  AText: string;
begin
  if ActiveProperties.IsMasked and not IsLoading then
  begin
    AText := VarToStr(Value);

    if not Focused and (Mode is TcxMaskEditStandardMode) then
    begin
      if not ActiveProperties.AlwaysShowBlanksAndLiterals then
        inherited InternalSetDisplayValue(Mode.GetUpdatedText(AText))
      else
        inherited InternalSetDisplayValue(Mode.GetFormattedText(AText));
    end
    else
      inherited InternalSetDisplayValue(Mode.GetFormattedText(AText));

    InternalSetValue(Text);
  end
  else
    inherited InternalSetDisplayValue(Value);
end;

function TcxCustomMaskEdit.InternalSetText(const Value: string): Boolean;
begin
  if not IsLoading and ActiveProperties.IsMasked then
  begin
    Result := True;
    InternalEditValue := Value;
    FEditValue := FText;
  end
  else
    Result := inherited InternalSetText(Value);
end;

function TcxCustomMaskEdit.IsTextInputMode: Boolean;
begin
  Result := (ActiveProperties.EditMask = '') and inherited IsTextInputMode;
end;

procedure TcxCustomMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not FMyMessage then
    inherited KeyDown(Key, Shift);
end;

procedure TcxCustomMaskEdit.KeyPress(var Key: Char);
begin
  if not FMyMessage then
    inherited KeyPress(Key);
end;

procedure TcxCustomMaskEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if not FMyMessage then
    inherited KeyUp(Key, Shift);
end;

procedure TcxCustomMaskEdit.Loaded;
begin
  inherited Loaded;
  if ActiveProperties.IsMasked then
  begin
    LockChangeEvents(True, False);
    try
      UpdateTextFormatting;
    finally
      LockChangeEvents(False, False);
    end;
  end;
end;

procedure TcxCustomMaskEdit.LockInnerEditRepainting;
begin
  if HasInnerEdit then
    if ActiveProperties.IsMasked then
      SendMessage(InnerEdit.Control.Handle, WM_SETREDRAW, 0, 0)
    else
      inherited LockInnerEditRepainting;
end;

procedure TcxCustomMaskEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ActiveProperties.IsMasked then
    if Button = mbLeft then
      Mode.LMouseDown;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TcxCustomMaskEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AShiftOn: Boolean;
begin
  if ActiveProperties.IsMasked then
    if ssLeft in Shift then
    begin
      AShiftOn := FShiftOn;
      FShiftOn := True;
      Mode.LMouseDown;
      FShiftOn := AShiftOn;
    end;

  inherited MouseMove(Shift, X, Y);
end;

procedure TcxCustomMaskEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TcxCustomMaskEdit.PropertiesChanged(Sender: TObject);
var
  AEditMask: string;
  ASelStart: Integer;
  ASelLength: Integer;
  AEditMaskCleared: Boolean;

  procedure DoMaskChanged;
  begin
    if ActiveProperties.IsMasked then
      ActiveProperties.AssignedValues.MaxLength := False;

    FMode.Compile(AEditMask);
    if AEditMaskCleared then
      ActiveProperties.FEditMask := AEditMask;
    if not ActiveProperties.EmptyMask(AEditMask) then
      FMode.SynchronizeEditValue
    else
    begin
      if FMode is TcxMaskEditStandardMode then
      begin
        if HandleAllocated and (SelLength = 1) then
        begin
          ASelStart := SelStart;
          SelLength := 0;
          SelStart := ASelStart;
        end;
      end;
    end;
    ActiveProperties.FEmptyString := InternalGetEmptyString;
  end;

  procedure DoMaskKindChanged;
  var
    APrevMaskKind: TcxEditMaskKind;
    APrevEditMask: string;
  begin
    APrevMaskKind := FMode.GetMaskKind;
    APrevEditMask := FMode.EditMask;
    FreeAndNil(FMode);

    if ActiveProperties.MaskKind = emkRegExpr then
    begin
      if APrevMaskKind = emkStandard then
      begin
        ActiveProperties.FEditMask := '';
        AEditMaskCleared := True;
      end;
      FMode := ActiveProperties.GetModeClass(emkRegExpr).Create(Self, ActiveProperties)
    end
    else if ActiveProperties.MaskKind = emkRegExprEx then
    begin
      if APrevMaskKind = emkStandard then
      begin
        ActiveProperties.FEditMask := '';
        AEditMaskCleared := True;
      end;
      FMode := ActiveProperties.GetModeClass(emkRegExprEx).Create(Self, ActiveProperties)
    end
    else if ActiveProperties.MaskKind = emkStandard then
    begin
      if (APrevMaskKind = emkRegExpr) or (APrevMaskKind = emkRegExprEx) then
      begin
        ActiveProperties.FEditMask := '';
        AEditMaskCleared := True;
      end;
      FMode := ActiveProperties.GetModeClass(emkStandard).Create(Self, ActiveProperties)
    end;

    FMode.Compile(ActiveProperties.EditMask);
    if not ActiveProperties.EmptyMask(ActiveProperties.EditMask) then
      FMode.SynchronizeEditValue;

    ActiveProperties.FEmptyString := InternalGetEmptyString;
  end;

  procedure DoCharCaseChanged;
  begin
    if ActiveProperties.CharCase = ecNormal then
    begin
      if HasInnerEdit then
        InnerTextEdit.CharCase := ecNormal;
      FMode.SynchronizeEditValue;
    end;
    FMode.CharCase := ActiveProperties.CharCase;
  end;

  procedure SaveSels;
  begin
    ASelStart := SelStart;
    ASelLength := SelLength;
  end;

  procedure LoadSels;
  begin
    SelStart := ASelStart;
    SelLength := ASelLength;
  end;

var
  AEditMaskChanged: Boolean;
  AEditMaskKindChanged: Boolean;
  ACharCaseChanged: Boolean;
begin
  AEditMaskCleared := False;
  ActiveProperties.LockUpdate(True);

  AEditMask := ActiveProperties.EditMask;
  AEditMaskChanged := (FMode.EditMask <> ActiveProperties.EditMask) or ActiveProperties.FormatChanging;
  AEditMaskKindChanged := FMode.GetMaskKind <> ActiveProperties.MaskKind;
  ACharCaseChanged := ActiveProperties.CharCase <> FMode.CharCase;
  if AEditMaskKindChanged then
    DoMaskKindChanged;
  if AEditMaskChanged then
    DoMaskChanged;
  if ACharCaseChanged then
    DoCharCaseChanged;
  if (ActiveProperties.EchoMode <> FMode.EchoMode) and HandleAllocated then
    SaveSels;

  if not(not PropertiesChangeLocked and VarIsNull(EditValue)) and ActiveProperties.IsMasked then
    FMode.UpdateEditValue;

  ActiveProperties.LockUpdate(False);

  if FMode is TcxMaskEditRegExprMode then
    TcxMaskEditRegExprMode(FMode).SetRegExprCaseInsensitive;

  inherited PropertiesChanged(Sender);

  if ActiveProperties.EchoMode <> FMode.EchoMode then
  begin
    if HandleAllocated then
      LoadSels;
    FMode.EchoMode := ActiveProperties.EchoMode;
  end;

  if ACharCaseChanged or AEditMaskKindChanged or AEditMaskChanged then
    FText := FMode.GetUpdatedText(InnerEdit.EditValue);
end;

procedure TcxCustomMaskEdit.PrepareEditValue(const ADisplayValue: TcxEditValue;
  out EditValue: TcxEditValue; AEditFocused: Boolean);
begin
  if ActiveProperties.IsMasked then
    if not AEditFocused and (Mode is TcxMaskEditStandardMode) and
      (TcxMaskEditStandardMode(Mode).FMask.SaveLiteralCharacters or not ActiveProperties.AlwaysShowBlanksAndLiterals) then
        inherited PrepareEditValue(ADisplayValue, EditValue, AEditFocused)
    else
      EditValue := Mode.GetUpdatedText(ADisplayValue)
  else
    inherited PrepareEditValue(ADisplayValue, EditValue, AEditFocused);
end;

procedure TcxCustomMaskEdit.PasteFromClipboard;
begin
  if not Clipboard.HasFormat(CF_TEXT) then
    Exit;

  if not ActiveProperties.IsMasked then
  begin
    inherited PasteFromClipboard;
    Exit;
  end;

  Mode.ClipboardTextLength := Length(Clipboard.AsText);
  Mode.PressDelete;
  Mode.PrePasteFromClipboard;

//    inherited PasteFromClipboard;
  SelText := Clipboard.AsText; // TODO

  Mode.AfterPasteFromClipboard;
end;

procedure TcxCustomMaskEdit.SelChange(Sender: TObject);
begin
  inherited SelChange(Sender);
  if not FInternalTextSelection then
    Mode.LMouseDown;
end;

procedure TcxCustomMaskEdit.SetSelText(const Value: TCaption);
begin
  if not ActiveProperties.IsMasked then
    inherited SetSelText(Value)
  else
  begin
    Mode.ClipboardTextLength := Length(Value);
    Mode.PrePasteFromClipboard;

    inherited SetSelText(Value);

    Mode.AfterPasteFromClipboard;
  end;
end;

procedure TcxCustomMaskEdit.SynchronizeDisplayValue;
begin
  inherited SynchronizeDisplayValue;

  if ActiveProperties.IsMasked then
    InternalSetValue(Text);
end;

procedure TcxCustomMaskEdit.UndoPerformed;
begin
  InternalSetValue(Text);
end;

procedure TcxCustomMaskEdit.UnlockInnerEditRepainting;
begin
  if HasInnerEdit then
    if ActiveProperties.IsMasked then
    begin
      SendMessage(InnerEdit.Control.Handle, WM_SETREDRAW, 1, 0);
      InnerEdit.Control.Invalidate;
    end
    else
      inherited UnlockInnerEditRepainting;
end;

function TcxCustomMaskEdit.CanChangeSelText(const Value: string;
  out ANewText: string; out ANewSelStart: Integer): Boolean;
begin
  Result := (ActiveProperties.IsMasked and Mode.ProcessSelText(Value, ANewText, ANewSelStart)) or
    inherited CanChangeSelText(Value, ANewText, ANewSelStart);
end;

function TcxCustomMaskEdit.CanSynchronizeModeText: Boolean;
begin
  Result := True;
end;

procedure TcxCustomMaskEdit.DirectSetSelLength(AValue: Integer);
begin
  FInternalTextSelection := True;
  try
    SelLength := AValue;
  finally
    FInternalTextSelection := False;
  end;
end;

procedure TcxCustomMaskEdit.DirectSetSelStart(AValue: Integer);
begin
  FInternalTextSelection := True;
  try
    SelStart := AValue;
  finally
    FInternalTextSelection := False;
  end;
end;

function TcxCustomMaskEdit.IsCharValidForPos(var AChar: Char;
  APos: Integer): Boolean;
begin
  Result := True;
end;

procedure TcxCustomMaskEdit.UpdateTextFormatting;
var
  ADisplayValue: Variant;
begin
  PrepareDisplayValue(EditValue, ADisplayValue, Focused);
  InternalSetDisplayValue(ADisplayValue);
end;

{ TcxMaskEdit }

function TcxMaskEdit.SupportsSpelling: Boolean;
begin
  Result := IsTextInputMode;
end;

class function TcxMaskEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMaskEditProperties;
end;

function TcxMaskEdit.GetActiveProperties: TcxMaskEditProperties;
begin
  Result := TcxMaskEditProperties(InternalGetActiveProperties);
end;

function TcxMaskEdit.GetProperties: TcxMaskEditProperties;
begin
  Result := TcxMaskEditProperties(inherited Properties);
end;

procedure TcxMaskEdit.SetProperties(Value: TcxMaskEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterMaskEditHelper }

class function TcxFilterMaskEditHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TcxMaskEdit;
end;

class function TcxFilterMaskEditHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties;
  AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoEqual, fcoNotEqual, fcoLess, fcoLessEqual,
    fcoGreater, fcoGreaterEqual, fcoBlanks, fcoNonBlanks];
  if not TcxCustomMaskEditProperties(AProperties).IsMasked and
    ((AValueTypeClass <> nil) and AValueTypeClass.IsString) then
    Result := Result + [fcoLike, fcoNotLike, fcoContains, fcoNotContains, fcoBeginsWith, fcoEndsWith];
  if AExtendedSet then
    Result := Result + [fcoBetween..fcoNotInList];
end;

class procedure TcxFilterMaskEditHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  with TcxCustomMaskEditProperties(AProperties) do
  begin
    if TcxCustomMaskEditProperties(AEditProperties).MaskKind =
        emkRegExpr then
      MaskKind := emkRegExprEx;
    AutoSelect := not IsMasked;
  end;
end;

initialization
  GetRegisteredEditProperties.Register(TcxMaskEditProperties, scxSEditRepositoryMaskItem);
  FilterEditsController.Register(TcxCustomMaskEditProperties, TcxFilterMaskEditHelper);

finalization
  FilterEditsController.Unregister(TcxCustomMaskEditProperties, TcxFilterMaskEditHelper);

end.
