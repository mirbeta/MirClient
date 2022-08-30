{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpellChecker                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPELLCHECKER AND ALL           }
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

unit dxSpellCheckerAdapters;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, Controls, Generics.Defaults, Generics.Collections, ComCtrls,
  cxClasses, dxCoreClasses, cxTextEdit, StdCtrls, dxSpellCheckerCore, cxRichEdit, cxRichEditUtils,
  dxSpellCheckerTextParsers;

type

  { IdxSpellCheckerAutoCorrectAdapter }

  IdxSpellCheckerAutoCorrectAdapter = interface(IdxSpellCheckerAdapter)
  ['{71BDB54A-C341-4FF5-95E0-78B25DD575BD}']
    function GetCursorPosition: IdxSpellCheckerPosition;
    function IgnoreCaretReturn: Boolean;
    procedure ApplyChanges(const AInfo: TdxSpellCheckerAutoCorrectWordInfo);
  end;

  { IdxSpellCheckerCheckAsYouTypeAdapter }

  TdxSpellCheckerCheckAsYouTypeAddUnderlineRectCallback = reference to procedure(const R: TRect);

  IdxSpellCheckerCheckAsYouTypeAdapter = interface(IdxSpellCheckerAdapter)
  ['{DC9C07C1-6D88-4B80-9FC2-8C0332533974}']
    procedure CalculateBounds(const AStart, AFinish: IdxSpellCheckerPosition;
      ACallback: TdxSpellCheckerCheckAsYouTypeAddUnderlineRectCallback);
    function GetIsLineDrawStyleNeeded: Boolean;
    function GetIsMultiLine: Boolean;
    function GetReadOnly: Boolean;
    procedure GetVisibleTextBounds(out AStartIndex, AEndIndex: IdxSpellCheckerPosition);
    procedure RefreshParams;

    property IsLineDrawStyleNeeded: Boolean read GetIsLineDrawStyleNeeded;
    property IsMultiLine: Boolean read GetIsMultiLine;
    property ReadOnly: Boolean read GetReadOnly;
  end;

  { TdxSpellCheckerCustomEditAdapter }

  TdxSpellCheckerCustomEditAdapter = class(TInterfacedObject, IdxSpellCheckerAdapter)
  protected
    FEdit: TWinControl;
    FNeedImmediatePost: Boolean;
    FSelStart: Integer;
    FSelLength: Integer;
    FText: string;

    function CreateController: IdxSpellCheckTextController; virtual;
    function GetEdit: TWinControl;
    function GetEditorHandle: THandle; virtual;
    function GetHideSelection: Boolean; virtual; abstract;
    function GetReadOnly: Boolean; virtual; abstract;
    function GetSelLength: Integer; virtual; abstract;
    function GetSelStart: Integer; virtual; abstract;
    function GetSelText: string; virtual; abstract;
    function GetText: string; virtual;
    procedure SetHideSelection(AValue: Boolean); virtual; abstract;
    procedure SetSelLength(AValue: Integer); virtual; abstract;
    procedure SetSelStart(AValue: Integer); virtual; abstract;
    procedure SetSelText(const AValue: string); virtual; abstract;
    function IsInplace: Boolean;

    class function CreateAdapter(AObject: TObject): IdxSpellCheckerAdapter;
  public
    constructor Create(AEdit: TWinControl); virtual;
    function EditorHandleAllocated: Boolean;
    procedure GetSelection(out AStart, AFinish: IdxSpellCheckerPosition);
    procedure GetSpellingBounds(out ASpellingStart, ASpellingEnd: Integer); overload; virtual;
    procedure GetSpellingBounds(out AStart, AFinish: IdxSpellCheckerPosition); overload;
    procedure Post(AUpdateValue: Boolean = True); virtual;
    procedure Replace(var AStart, AFinish: IdxSpellCheckerPosition; const AWord: string;
      var ASpellingStart, ASpellingFinish: IdxSpellCheckerPosition);
    procedure SetSelection(const AStart, AFinish: IdxSpellCheckerPosition);
    procedure UpdateController(AController: IdxSpellCheckTextController);

    property Edit: TWinControl read GetEdit;
    property EditorHandle: THandle read GetEditorHandle;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection;
    property ReadOnly: Boolean read GetReadOnly;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
    property Text: string read GetText;
  end;

  { TdxSpellCheckerCxTextEditAdapter }

  TdxSpellCheckerCxTextEditAdapter = class(TdxSpellCheckerCustomEditAdapter,
    IdxSpellCheckerAutoCorrectAdapter,
    IdxSpellCheckerCheckAsYouTypeAdapter)
  strict private
    FLineHeight: Integer;

    function GetEditEx: TCustomEdit;
  protected
    FIsLineDrawStyleNeeded: Boolean;

    function GetHideSelection: Boolean; override;
    function GetReadOnly: Boolean; override;
    function GetSelLength: Integer; override;
    function GetSelStart: Integer; override;
    function GetSelText: string; override;
    procedure SetHideSelection(AValue: Boolean); override;
    procedure SetSelLength(AValue: Integer); override;
    procedure SetSelStart(AValue: Integer); override;
    procedure SetSelText(const AValue: string); override;

    procedure CalculateBounds(AStartIndex, AFinishIndex: Integer;
      ACallback: TdxSpellCheckerCheckAsYouTypeAddUnderlineRectCallback); overload; virtual;
    function GetCharIndex(const APoint: TPoint): Integer; virtual;
    function GetCharPosition(ACharIndex: Integer): TPoint; virtual;
    function GetFirstCharIndexInLine(ALineIndex: Integer): Integer; virtual;
    function GetFirstVisibleLineIndex: Integer; virtual;
    function GetLineIndex(ACharIndex: Integer): Integer; virtual;
    function GetLineLength(ALineIndex: Integer): Integer; virtual;
    function GetTextAreaRect: TRect; virtual;
    function GetUnderlineRect(APosition, ALength: Integer): TRect; virtual;
    procedure GetVisibleTextBounds(out AStartIndex, AEndIndex: Integer); overload; virtual;
    function InternalGetLineHeightByFont(ALineIndex: Integer): Integer; virtual;

    // IdxSpellCheckerAutoCorrectAdapter
    function GetCursorPosition: IdxSpellCheckerPosition;
    function IgnoreCaretReturn: Boolean; virtual;
    procedure ApplyChanges(const AInfo: TdxSpellCheckerAutoCorrectWordInfo);

    // IdxSpellCheckerCheckAsYouTypeAdapter
    procedure CalculateBounds(const AStart, AFinish: IdxSpellCheckerPosition;
      ACallback: TdxSpellCheckerCheckAsYouTypeAddUnderlineRectCallback); overload;
    function GetIsLineDrawStyleNeeded: Boolean; virtual;
    function GetIsMultiLine: Boolean; virtual;
    procedure GetVisibleTextBounds(out AStartIndex, AEndIndex: IdxSpellCheckerPosition); overload;
    procedure RefreshParams; virtual;

    property Edit: TCustomEdit read GetEditEx;
    property IsMultiLine: Boolean read GetIsMultiLine;
    property TextAreaRect: TRect read GetTextAreaRect;
  public
    constructor Create(AEdit: TWinControl); override;
  end;

  { TdxSpellCheckerCxRichEditAdapter }

  TdxSpellCheckerCxRichEditAdapter = class(TdxSpellCheckerCxTextEditAdapter)
  strict private
    FRichVersion: Integer;

    function GetEditEx: TcxRichInnerEdit; inline;
  protected
    function GetCharIndex(const APoint: TPoint): Integer; override;
    function GetCharPosition(ACharIndex: Integer): TPoint; override;
    function GetText: string; override;
    function GetUnderlineRect(APosition, ALength: Integer): TRect; override;
    function InternalGetLineHeightByFont(ALineIndex: Integer): Integer; override;
    function IgnoreCaretReturn: Boolean; override;
  public
    constructor Create(AEdit: TWinControl); override;
    //
    property Edit: TcxRichInnerEdit read GetEditEx;
    property RichVersion: Integer read FRichVersion;
  end;

  { TdxSpellCheckerEditAdapter }

  TdxSpellCheckerEditAdapter = class(TdxSpellCheckerCustomEditAdapter)
  strict private
    function GetEdit: TCustomEdit;
  protected
    function GetHideSelection: Boolean; override;
    function GetReadOnly: Boolean; override;
    function GetSelLength: Integer; override;
    function GetSelStart: Integer; override;
    function GetSelText: string; override;
    procedure SetHideSelection(AValue: Boolean); override;
    procedure SetSelLength(AValue: Integer); override;
    procedure SetSelStart(AValue: Integer); override;
    procedure SetSelText(const AValue: string); override;

    property Edit: TCustomEdit read GetEdit;
  end;

  { TdxSpellCheckerRichEditAdapter }

  TdxSpellCheckerRichEditAdapter = class(TdxSpellCheckerEditAdapter)
  protected
    function GetText: string; override;
  end;

  { TdxSpellCheckerTextAdapter }

  TdxSpellCheckerTextAdapter = class(TdxSpellCheckerEditAdapter)
  public
    constructor Create(const AText: string); reintroduce; virtual;
  end;

implementation

uses
  dxCore, SysUtils, cxContainer, cxEdit, Math, cxGraphics, cxGeometry, dxDrawRichTextUtils, cxMemo,
  dxMessages, dxCoreGraphics;

type
  TcxRichInnerEditAccess = class(TcxRichInnerEdit);
  TCustomEditAccess = class(TCustomEdit);

var
  dxSpellCheckerAutoCorrectWord: TdxSpellCheckerAutoCorrectWordRange;

function GetAdjustedRichEditText(const AText: string; AUseShortLineBreak: Boolean): string;
var
  ALen: Integer;
begin
  ALen := Length(AText);
  SetLength(Result, ALen);
  if ALen > 0 then
  begin
    ALen := AdjustRichLineBreaksW(PWideChar(Result), PWideChar(AText), AUseShortLineBreak);
    SetLength(Result, ALen);
  end;
end;

{ TdxSpellCheckerCustomEditAdapter }

constructor TdxSpellCheckerCustomEditAdapter.Create(AEdit: TWinControl);
begin
  inherited Create;
  FEdit := AEdit;
  FNeedImmediatePost := (AEdit <> nil) and not AEdit.Focused;
end;

procedure TdxSpellCheckerCustomEditAdapter.GetSpellingBounds(out AStart, AFinish: IdxSpellCheckerPosition);
var
  ASpellingStart, ASpellingEnd: Integer;
begin
  GetSpellingBounds(ASpellingStart, ASpellingEnd);
  AStart := TdxSpellCheckerIntegerPosition.Create(ASpellingStart);
  AFinish := TdxSpellCheckerIntegerPosition.Create(ASpellingEnd);
end;

procedure TdxSpellCheckerCustomEditAdapter.GetSelection(out AStart, AFinish: IdxSpellCheckerPosition);
begin
  if EditorHandleAllocated then
  begin
    AFinish := TdxSpellCheckerIntegerPosition.Create(SelStart + SelLength);
    AStart := TdxSpellCheckerIntegerPosition.Create(SelStart);
  end
  else
  begin
    AFinish := TdxSpellCheckerIntegerPosition.Create(0);
    AStart := TdxSpellCheckerIntegerPosition.Create(0);
  end;
end;

procedure TdxSpellCheckerCustomEditAdapter.GetSpellingBounds(out ASpellingStart, ASpellingEnd: Integer);
begin
  ASpellingStart := 0;
  if EditorHandleAllocated then
    ASpellingEnd := SendMessageW(EditorHandle, WM_GETTEXTLENGTH, 0, 0)
  else
    ASpellingEnd := Length(FText) - 1;
end;

function TdxSpellCheckerCustomEditAdapter.EditorHandleAllocated: Boolean;
begin
  Result := (FEdit <> nil) and FEdit.HandleAllocated
end;

function TdxSpellCheckerCustomEditAdapter.CreateController: IdxSpellCheckTextController;
begin
  Result := TdxSpellCheckerTextController.Create(Text);
end;

function TdxSpellCheckerCustomEditAdapter.GetEdit: TWinControl;
begin
  Result := FEdit;
end;

function TdxSpellCheckerCustomEditAdapter.GetEditorHandle: THandle;
begin
  if EditorHandleAllocated then
    Result := FEdit.Handle
  else
    Result := 0;
end;

function TdxSpellCheckerCustomEditAdapter.GetText: string;
var
  ALen: Integer;
  ATemp: AnsiString;
begin
  if EditorHandleAllocated then
  begin
    if IsWindowUnicode(EditorHandle) then
    begin
      ALen := SendMessageW(EditorHandle, WM_GETTEXTLENGTH, 0, 0) + 1;
      SetLength(FText, ALen);
      GetWindowTextW(EditorHandle, Pointer(FText), ALen);
    end
    else
    begin
      ALen := SendMessageA(EditorHandle, WM_GETTEXTLENGTH, 0, 0) + 1;
      SetLength(ATemp, ALen);
      GetWindowTextA(EditorHandle, Pointer(ATemp), ALen);
      FText := dxAnsiStringToString(ATemp);
    end;
    SetLength(FText, ALen - 1);
  end;
  Result := FText;
end;

procedure TdxSpellCheckerCustomEditAdapter.Post(AUpdateValue: Boolean = True);
var
  AEdit: IdxSpellCheckerControl;
begin
  if Supports(GetInnerControlContainer(FEdit), IdxSpellCheckerControl, AEdit) and AUpdateValue and not EditorHandleAllocated then
    AEdit.SetValue(FText);
end;

procedure TdxSpellCheckerCustomEditAdapter.Replace(
  var AStart, AFinish: IdxSpellCheckerPosition; const AWord: string;
  var ASpellingStart, ASpellingFinish: IdxSpellCheckerPosition);
var
  ADelta: Integer;
  ASelStart: Integer;
begin
  SetSelection(AStart, AFinish);
  ASelStart := SelStart;
  ADelta := Length(AWord) - SelLength;
  SelText := AWord;
  AFinish := TdxSpellCheckerIntegerPosition.Create(ASelStart + Length(AWord));

  if (ADelta <> 0) and (ASpellingStart <> nil) and (ASpellingFinish <> nil) then
  begin
    if AStart.Compare(ASpellingStart) < 0 then
      ASpellingStart := ASpellingStart.Add(TdxSpellCheckerIntegerPosition.Create(ADelta));
    if AStart.Compare(ASpellingFinish) < 0 then
      ASpellingFinish := ASpellingFinish.Add(TdxSpellCheckerIntegerPosition.Create(ADelta));
  end;
end;

procedure TdxSpellCheckerCustomEditAdapter.SetSelection(const AStart, AFinish: IdxSpellCheckerPosition);
begin
  SelStart := AStart.ToInteger;
  SelLength := AFinish.ToInteger - SelStart;
end;

procedure TdxSpellCheckerCustomEditAdapter.UpdateController(AController: IdxSpellCheckTextController);
var
  ATextController: TdxSpellCheckerTextController;
begin
  ATextController := TdxSpellCheckerTextController(AController);
  if ATextController <> nil then
    ATextController.Text := Text;
end;

function TdxSpellCheckerCustomEditAdapter.IsInplace: Boolean;
begin
  Result := TcxCustomEdit(GetInnerControlContainer(FEdit)).IsInplace;
end;

class function TdxSpellCheckerCustomEditAdapter.CreateAdapter(AObject: TObject): IdxSpellCheckerAdapter;
begin
  Result := Create(AObject as TWinControl);
end;

{ TdxSpellCheckerCxTextEditAdapter }

constructor TdxSpellCheckerCxTextEditAdapter.Create(AEdit: TWinControl);
begin
  if AEdit is TcxContainer then
    AEdit := TcxContainer(AEdit).InnerControl;
  inherited Create(AEdit);
end;

function TdxSpellCheckerCxTextEditAdapter.GetHideSelection: Boolean;
var
  AContainer: TWinControl;
begin
  AContainer := GetInnerControlContainer(Edit);
  if AContainer is TcxCustomTextEdit then
    Result := TcxCustomTextEdit(AContainer).Properties.HideSelection
  else
    Result := TCustomEditAccess(Edit).HideSelection;
end;

function TdxSpellCheckerCxTextEditAdapter.GetReadOnly: Boolean;
begin
  Result := Edit.ReadOnly;
end;

function TdxSpellCheckerCxTextEditAdapter.GetSelLength: Integer;
begin
  Result := Edit.SelLength;
end;

function TdxSpellCheckerCxTextEditAdapter.GetSelStart: Integer;
begin
  Result := Edit.SelStart
end;

function TdxSpellCheckerCxTextEditAdapter.GetSelText: string;
begin
  Result := Edit.SelText
end;

procedure TdxSpellCheckerCxTextEditAdapter.SetHideSelection(AValue: Boolean);
var
  AContainer: TWinControl;
begin
  AContainer := GetInnerControlContainer(Edit);
  if AContainer is TcxCustomTextEdit then
    TcxCustomTextEdit(AContainer).Properties.HideSelection := AValue
  else
    TCustomEditAccess(Edit).HideSelection := AValue;
end;

procedure TdxSpellCheckerCxTextEditAdapter.SetSelLength(AValue: Integer);
begin
  Edit.SelLength := AValue;
end;

procedure TdxSpellCheckerCxTextEditAdapter.SetSelStart(AValue: Integer);
begin
  Edit.SelStart := AValue;
end;

procedure TdxSpellCheckerCxTextEditAdapter.SetSelText(const AValue: string);
var
  AIntf: IdxSpellCheckerControl;
begin
  if Supports(GetInnerControlContainer(Edit), IdxSpellCheckerControl, AIntf) then
    AIntf.SetSelText(AValue, FNeedImmediatePost)
  else
    Edit.SelText := AValue;
end;

procedure TdxSpellCheckerCxTextEditAdapter.CalculateBounds(AStartIndex, AFinishIndex: Integer;
  ACallback: TdxSpellCheckerCheckAsYouTypeAddUnderlineRectCallback);
var
  ACharIndex, AWordPartLength: Integer;
  AFinishLineIndex: Integer;
  AFirstLineCharIndex: Integer;
  AStartLineIndex: Integer;
  I: Integer;
begin
  AStartLineIndex := GetLineIndex(AStartIndex);
  AFinishLineIndex := GetLineIndex(AFinishIndex - 1);
  for I := AStartLineIndex to AFinishLineIndex do
  begin
    AFirstLineCharIndex := GetFirstCharIndexInLine(I);
    if I = AStartLineIndex then
      ACharIndex := AStartIndex
    else
      ACharIndex := AFirstLineCharIndex;

    if I = AFinishLineIndex then
      AWordPartLength := AFinishIndex - ACharIndex
    else
      AWordPartLength := GetLineLength(I) - ACharIndex + AFirstLineCharIndex;

    ACallback(GetUnderlineRect(ACharIndex, AWordPartLength));
  end;
end;

function TdxSpellCheckerCxTextEditAdapter.GetCharIndex(const APoint: TPoint): Integer;
begin
  Result := LoWord(SendMessage(EditorHandle, EM_CHARFROMPOS, 0, MakeLParam(APoint.X, APoint.Y)));
  if Result = MAXWORD then
    Result := -1;
end;

function TdxSpellCheckerCxTextEditAdapter.GetCharPosition(ACharIndex: Integer): TPoint;
var
  APosition: Integer;
begin
  APosition := SendMessage(EditorHandle, EM_POSFROMCHAR, ACharIndex, 0);
  Result.X := SmallInt(LoWord(APosition));
  Result.Y := SmallInt(HiWord(APosition));
end;

function TdxSpellCheckerCxTextEditAdapter.GetFirstCharIndexInLine(ALineIndex: Integer): Integer;
begin
  Result := SendMessage(EditorHandle, EM_LINEINDEX, ALineIndex, 0);
end;

function TdxSpellCheckerCxTextEditAdapter.GetFirstVisibleLineIndex: Integer;
begin
  Result := SendMessage(EditorHandle, EM_GETFIRSTVISIBLELINE, 0, 0);
end;

function TdxSpellCheckerCxTextEditAdapter.GetLineIndex(ACharIndex: Integer): Integer;
begin
  Result := SendMessage(EditorHandle, EM_LINEFROMCHAR, ACharIndex, 0);
end;

function TdxSpellCheckerCxTextEditAdapter.GetLineLength(ALineIndex: Integer): Integer;
begin
  Result := SendMessage(EditorHandle, EM_LINELENGTH, GetFirstCharIndexInLine(ALineIndex), 0);
end;

function TdxSpellCheckerCxTextEditAdapter.GetTextAreaRect: TRect;
begin
  SendMessage(EditorHandle, EM_GETRECT, 0, LPARAM(@Result));
end;

function TdxSpellCheckerCxTextEditAdapter.GetUnderlineRect(APosition, ALength: Integer): TRect;

  function GetTextWidth(APosition, ALength: Integer): Integer;
  var
    AOldFont: HFONT;
    AText: string;
    ATextSize: TSize;
    DC: HDC;
  begin
    DC := GetDC(EditorHandle);
    AOldFont := SelectObject(DC, SendMessage(EditorHandle, WM_GETFONT, 0, 0));
    AText := Copy(Text, APosition + 1, ALength);
    GetTextExtentPointW(DC, PWideChar(AText), Length(AText), ATextSize);
    Result := ATextSize.cx;
    SelectObject(DC, AOldFont);
    ReleaseDC(EditorHandle, DC);
  end;

begin
  Result.TopLeft := GetCharPosition(APosition);
  Result.Bottom := Result.Top + FLineHeight;
  Result.Right := Result.Left + GetTextWidth(APosition, ALength);
end;

procedure TdxSpellCheckerCxTextEditAdapter.GetVisibleTextBounds(out AStartIndex, AEndIndex: Integer);
var
  R: TRect;
begin
  R := TextAreaRect;
  InflateRect(R, -1, -1);
  AStartIndex := Max(0, GetCharIndex(R.TopLeft));
  AEndIndex := GetCharIndex(R.BottomRight);
end;

function TdxSpellCheckerCxTextEditAdapter.InternalGetLineHeightByFont(ALineIndex: Integer): Integer;
begin
  Result := cxTextExtent(SendMessage(EditorHandle, WM_GETFONT, 0, 0), dxMeasurePattern).cy;
end;

function TdxSpellCheckerCxTextEditAdapter.GetCursorPosition: IdxSpellCheckerPosition;
begin
  Result := TdxSpellCheckerIntegerPosition.Create(SelStart);
end;

function TdxSpellCheckerCxTextEditAdapter.IgnoreCaretReturn: Boolean;
begin
  Result := False;
end;

procedure TdxSpellCheckerCxTextEditAdapter.ApplyChanges(const AInfo: TdxSpellCheckerAutoCorrectWordInfo);
begin
  dxSpellCheckerAutoCorrectWord := AInfo.ToWordRange;
  PostMessageW(GetInnerControlContainer(Edit).Handle, DXM_SPELL_AUTOCORRECT, 0, LPARAM(@dxSpellCheckerAutoCorrectWord));
end;

procedure TdxSpellCheckerCxTextEditAdapter.CalculateBounds(const AStart, AFinish: IdxSpellCheckerPosition;
  ACallback: TdxSpellCheckerCheckAsYouTypeAddUnderlineRectCallback);
begin
  CalculateBounds(AStart.ToInteger, AFinish.ToInteger, ACallback);
end;

function TdxSpellCheckerCxTextEditAdapter.GetIsLineDrawStyleNeeded: Boolean;
begin
  Result := FIsLineDrawStyleNeeded;
end;

function TdxSpellCheckerCxTextEditAdapter.GetIsMultiLine: Boolean;
begin
  Result := SendMessage(EditorHandle, EM_GETLINECOUNT, 0, 0) > 1;
end;

procedure TdxSpellCheckerCxTextEditAdapter.GetVisibleTextBounds(out AStartIndex, AEndIndex: IdxSpellCheckerPosition);
var
  AStartIndexAsInteger, AEndIndexAsInteger: Integer;
begin
  GetVisibleTextBounds(AStartIndexAsInteger, AEndIndexAsInteger);
  AEndIndex := TdxSpellCheckerIntegerPosition.Create(AEndIndexAsInteger);
  AStartIndex := TdxSpellCheckerIntegerPosition.Create(AStartIndexAsInteger);
end;

procedure TdxSpellCheckerCxTextEditAdapter.RefreshParams;
begin
  FLineHeight := InternalGetLineHeightByFont(0);
  FIsLineDrawStyleNeeded := FLineHeight >= cxRectHeight(TextAreaRect);
end;

function TdxSpellCheckerCxTextEditAdapter.GetEditEx: TCustomEdit;
begin
  Result := inherited Edit as TCustomEdit;
end;

{ TdxSpellCheckerCxRichEditAdapter }

constructor TdxSpellCheckerCxRichEditAdapter.Create(AEdit: TWinControl);
begin
  inherited Create(AEdit);
  FRichVersion := TcxRichInnerEditAccess(Edit).RichVersion;
end;

function TdxSpellCheckerCxRichEditAdapter.GetCharIndex(const APoint: TPoint): Integer;
begin
  Result := SendMessage(EditorHandle, EM_CHARFROMPOS, 0, LParam(@APoint));
end;

function TdxSpellCheckerCxRichEditAdapter.GetCharPosition(ACharIndex: Integer): TPoint;
begin
  Result := cxRichEditGetCharPosition(EditorHandle, RichVersion, ACharIndex);
end;

function TdxSpellCheckerCxRichEditAdapter.GetText: string;
begin
  Result := GetAdjustedRichEditText(inherited GetText, RichVersion >= 200);
end;

function TdxSpellCheckerCxRichEditAdapter.GetUnderlineRect(APosition, ALength: Integer): TRect;

  function GetLineHeight(ACharIndex, ALineIndex: Integer; const ACharPosition: TPoint): Integer;
  begin
    if IsMultiLine then
    begin
      Result := GetFirstCharIndexInLine(ALineIndex + 1);
      if Result <> -1 then
      begin
        Result := GetCharPosition(Result).Y - ACharPosition.Y;
        if Result <= 0 then
          Result := InternalGetLineHeightByFont(ALineIndex);
      end
      else
        Result := InternalGetLineHeightByFont(ALineIndex);
    end
    else
      Result := InternalGetLineHeightByFont(0);
  end;

  function GetTextWidth(ACharIndex, ALineIndex: Integer; ALength: Integer; const ACharPosition: TPoint): Integer;
  begin
    if ALineIndex = GetLineIndex(ACharIndex + ALength) then
      Result := GetCharPosition(ACharIndex + ALength).X - ACharPosition.X
    else
      if ALength > 1 then
      begin
        Result := GetCharPosition(ACharIndex + ALength - 1).X - ACharPosition.X;
        Result := Result + Result div (ALength - 1);
      end
      else
        Result := 0;
  end;

var
  ALineIndex: Integer;
begin
  ALineIndex := GetLineIndex(APosition);
  Result.TopLeft := GetCharPosition(APosition);
  Result.Bottom := Result.Top + GetLineHeight(APosition, ALineIndex, Result.TopLeft);
  Result.Right := Result.Left + GetTextWidth(APosition, ALineIndex, ALength, Result.TopLeft);
end;

function TdxSpellCheckerCxRichEditAdapter.InternalGetLineHeightByFont(ALineIndex: Integer): Integer;
var
  ACharIndex: Integer;
  ADenominator: TdxNativeInt;
  ANumerator: TdxNativeInt;
  ARect: TRect;
  DC: HDC;
begin
  DC := GetDC(EditorHandle);
  try
    ARect := TextAreaRect;
    if not (IsMultiLine and Edit.WordWrap) then
      ARect.Right := dxTwipsToPixels(MaxInt);
    ACharIndex := GetFirstCharIndexInLine(ALineIndex);
    cxDrawRichEdit(DC, ARect, EditorHandle, ACharIndex, ACharIndex + GetLineLength(ALineIndex), True, Result);
    if Boolean(SendMessage(EditorHandle, EM_GETZOOM, WPARAM(@ANumerator), LPARAM(@ADenominator))) then
    begin
      if ADenominator > 0 then
        Result := MulDiv(Result, ANumerator, ADenominator);
    end;
  finally
    ReleaseDC(EditorHandle, DC);
  end;
end;

function TdxSpellCheckerCxRichEditAdapter.IgnoreCaretReturn: Boolean;
begin
  Result := True;
end;

function TdxSpellCheckerCxRichEditAdapter.GetEditEx: TcxRichInnerEdit;
begin
  Result := TcxRichInnerEdit(inherited Edit);
end;

{ TdxSpellCheckerEditAdapter }

function TdxSpellCheckerEditAdapter.GetHideSelection: Boolean;
begin
  if FEdit is TMemo then
    Result := TMemo(FEdit).HideSelection
  else
    if FEdit is TEdit then
      Result := TEdit(FEdit).HideSelection
    else
      if FEdit is TcxCustomRichEdit then
        Result := True
      else
        Result := False;
end;

function TdxSpellCheckerEditAdapter.GetReadOnly: Boolean;
begin
  Result := TEdit(FEdit).ReadOnly;
end;

function TdxSpellCheckerEditAdapter.GetSelLength: Integer;
begin
  if EditorHandleAllocated then
    Result := Edit.SelLength
  else
    Result := FSelLength;
end;

function TdxSpellCheckerEditAdapter.GetSelStart: Integer;
begin
  if EditorHandleAllocated then
    Result := Edit.SelStart
  else
    Result := FSelStart;
end;

function TdxSpellCheckerEditAdapter.GetSelText: string;
begin
  if EditorHandleAllocated then
    Result := Edit.SelText
  else
    Result := Copy(FText, FSelStart, FSelLength);
end;

procedure TdxSpellCheckerEditAdapter.SetHideSelection(AValue: Boolean);
begin
  if FEdit is TMemo then
    TMemo(FEdit).HideSelection := AValue
  else
    if FEdit is TEdit then
      TEdit(FEdit).HideSelection := AValue;
end;

procedure TdxSpellCheckerEditAdapter.SetSelLength(AValue: Integer);
begin
  if EditorHandleAllocated then
    Edit.SelLength := AValue;
  FSelLength := AValue;
end;

procedure TdxSpellCheckerEditAdapter.SetSelStart(AValue: Integer);
begin
  if EditorHandleAllocated then
    Edit.SelStart := AValue;
  FSelStart := AValue;
end;

procedure TdxSpellCheckerEditAdapter.SetSelText(const AValue: string);
var
  AIntf: IdxSpellCheckerControl;
begin
  FText := Copy(FText, 1, FSelStart) + AValue + Copy(FText, FSelStart + FSelLength + 1, Length(FText) - FSelStart - FSelLength);
  if EditorHandleAllocated then
  begin
    if Supports(GetInnerControlContainer(Edit), IdxSpellCheckerControl, AIntf) then
      AIntf.SetSelText(AValue, FNeedImmediatePost)
    else
      Edit.SelText := AValue;
  end;
end;

function TdxSpellCheckerEditAdapter.GetEdit: TCustomEdit;
begin
  Result := TCustomEdit(inherited Edit);
end;

{ TdxSpellCheckerRichEditAdapter }

function TdxSpellCheckerRichEditAdapter.GetText: string;
begin
  Result := GetAdjustedRichEditText(inherited GetText, True);
end;

{ TdxSpellCheckerTextAdapter }

constructor TdxSpellCheckerTextAdapter.Create(const AText: string);
begin
  inherited Create(nil);
  FText := AText;
end;

initialization
  TdxSpellCheckerAdapters.Register(TCustomEdit, TdxSpellCheckerEditAdapter.CreateAdapter);
  TdxSpellCheckerAdapters.Register(TCustomRichEdit, TdxSpellCheckerRichEditAdapter.CreateAdapter);
  TdxSpellCheckerAdapters.Register(TcxCustomTextEdit, TdxSpellCheckerCxTextEditAdapter.CreateAdapter);
  TdxSpellCheckerAdapters.Register(TcxCustomInnerMemo, TdxSpellCheckerCxTextEditAdapter.CreateAdapter);
  TdxSpellCheckerAdapters.Register(TcxCustomInnerTextEdit, TdxSpellCheckerCxTextEditAdapter.CreateAdapter);
  TdxSpellCheckerAdapters.Register(TcxCustomRichEdit, TdxSpellCheckerCxRichEditAdapter.CreateAdapter);
  TdxSpellCheckerAdapters.Register(TcxRichInnerEdit, TdxSpellCheckerCxRichEditAdapter.CreateAdapter);

finalization
  TdxSpellCheckerAdapters.Unregister(TCustomEdit);
  TdxSpellCheckerAdapters.Unregister(TCustomRichEdit);
  TdxSpellCheckerAdapters.Unregister(TcxCustomTextEdit);
  TdxSpellCheckerAdapters.Unregister(TcxCustomInnerMemo);
  TdxSpellCheckerAdapters.Unregister(TcxCustomInnerTextEdit);
  TdxSpellCheckerAdapters.Unregister(TcxCustomRichEdit);
  TdxSpellCheckerAdapters.Unregister(TcxRichInnerEdit);
end.
