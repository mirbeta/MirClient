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

unit dxRichEdit.Utils.Keyboard;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Windows, SysUtils, Classes, Controls, Generics.Defaults, Generics.Collections,
  dxCoreClasses;

type
  { TdxKeyEventArgs }

  TdxKeyEventArgs = record
    Alt: Boolean;
    AltGr: Boolean;
    Control: Boolean;
    Handled: Boolean;
    KeyData: Word;
    Modifiers: Boolean;
    Shift: Boolean;
    ShiftState: TShiftState;
    SuppressKeyPress: Boolean;
    constructor Create(AKey: Word; AShift: TShiftState);
  end;

  { TdxKeyPressEventArgs }

  TdxKeyPressEventArgs = record
    AltGr: Boolean;
    Key: Char;
    Handled: Boolean;
    Shift: TShiftState;
    constructor Create(AKey: Char; AShift: TShiftState);
  end;

  { IdxKeyboardHandlerService }

  IdxKeyboardHandlerService = interface
  ['{D139FA38-5357-4FD5-8CA3-F9D1853BB7F3}']
    function CanKeyDownModifyEdit(const Args: TdxKeyEventArgs): Boolean;
    function HandleKeyDown(const Args: TdxKeyEventArgs): Boolean;
    function HandleKeyUp(const Args: TdxKeyEventArgs): Boolean;
    function HandleKeyPress(const Args: TdxKeyPressEventArgs): Boolean;
  end;

  { TdxKeyboardHelper }

  TdxKeyboardHelper = class
  strict private
    class function CheckIsKeyPressed(const Index: Integer): Boolean; static;
    class function GetKeyState: TShiftState; static;
    class function GetModifierKeys: TShortCut; static;
  public
    class function ArgsToShortCut(const Args: TdxKeyEventArgs): TShortCut; static;
    class function CharToShortCut(AValue: Char; const AShiftState: TShiftState): TShortCut; static;
    class function KeysToShortCut(AKey: Word; const AShiftState: TShiftState): TShortCut; static;
    class function IsValidShortCutChar(AValue: Char; out AKey: Word): Boolean; static;

    class function GetKeyData(AShortCut: TShortCut): Word; static;
    class function GetShiftState(AShortCut: TShortCut): TShiftState; static;

    class property IsAltPressed: Boolean index VK_MENU read CheckIsKeyPressed;
    class property IsAltGrPressed: Boolean index VK_RMENU read CheckIsKeyPressed;
    class property IsControlPressed: Boolean index VK_CONTROL read CheckIsKeyPressed;
    class property IsShiftPressed: Boolean index VK_SHIFT read CheckIsKeyPressed;
    class property KeyState: TShiftState read GetKeyState;
    class property ModifierKeys: TShortCut read GetModifierKeys;
  end;

  { TdxCustomKeyboardController }

  TdxCustomKeyboardController = class abstract(TcxIUnknownObject)
  strict private
    FHandlers: TList<IdxKeyboardHandlerService>;
    FSuppressKeyPressHandling: Boolean;
    function GetHandler: IdxKeyboardHandlerService;
  protected
    property Handlers: TList<IdxKeyboardHandlerService> read FHandlers;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddHandler(const AHandler: IdxKeyboardHandlerService);
    procedure RemoveHandler(const AHandler: IdxKeyboardHandlerService);

    function CanKeyDownModifyEdit(Key: Word; Shift: TShiftState): Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key: Char); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;

    property Handler: IdxKeyboardHandlerService read GetHandler;
  end;

implementation

uses
  Forms, Menus;

{ TdxKeyEventArgs }

constructor TdxKeyEventArgs.Create(AKey: Word; AShift: TShiftState);
begin
  KeyData := AKey;
  Alt := ssAlt in AShift;
  Control := ssCtrl in AShift;
  Shift := ssShift in AShift;
  ShiftState := AShift;
  AltGr := TdxKeyboardHelper.IsAltGrPressed;
  Handled := False;
end;

{ TdxKeyPressEventArgs }

constructor TdxKeyPressEventArgs.Create(AKey: Char; AShift: TShiftState);
begin
  AltGr := TdxKeyboardHelper.IsAltGrPressed;
  Key := AKey;
  Shift := AShift;
  Handled := False;
end;

{ TdxKeyboardHelper }

class function TdxKeyboardHelper.ArgsToShortCut(const Args: TdxKeyEventArgs): TShortCut;
begin
  Result := KeysToShortCut(Args.KeyData, Args.ShiftState);
end;

class function TdxKeyboardHelper.CharToShortCut(AValue: Char; const AShiftState: TShiftState): TShortCut;
var
  AKey: Word;
begin
  if not IsValidShortCutChar(AValue, AKey) then
    Exit(0);
  Result := KeysToShortCut(AKey, AShiftState);
end;

class function TdxKeyboardHelper.CheckIsKeyPressed(const Index: Integer): Boolean;
begin
  Result := GetAsyncKeyState(Index) < 0;
end;

class function TdxKeyboardHelper.GetKeyState: TShiftState;
var
  AKeyState: TKeyboardState;
begin
  if GetKeyboardState(AKeyState) then
    Result := KeyboardStateToShiftState(AKeyState)
  else
    Result := [];
end;

class function TdxKeyboardHelper.GetModifierKeys: TShortCut;
begin
  Result := ShortCut(0, KeyState);
end;

class function TdxKeyboardHelper.KeysToShortCut(AKey: Word; const AShiftState: TShiftState): TShortCut;
begin
  Result := ShortCut(AKey, AShiftState);
end;

class function TdxKeyboardHelper.IsValidShortCutChar(AValue: Char; out AKey: Word): Boolean;
begin
  if Ord(AValue) >=32  then
  begin
    AKey := TextToShortCut(AValue);
    Result := AKey <> 0;
  end
  else
  begin
    AKey := Ord(AValue);
    Result := AKey in [$08, $09, $0D, $1B];
  end;
end;

class function TdxKeyboardHelper.GetKeyData(AShortCut: TShortCut): Word;
begin
  Result := AShortCut and not scAlt and not scCtrl and not scShift;
end;

class function TdxKeyboardHelper.GetShiftState(AShortCut: TShortCut): TShiftState;
begin
  Result := [];
  if AShortCut and scAlt = scAlt then
    Result := Result + [ssAlt];
  if AShortCut and scCtrl = scCtrl then
    Result := Result + [ssCtrl];
  if AShortCut and scShift = scShift then
    Result := Result + [ssShift];
end;

{ TdxCustomKeyboardController }

constructor TdxCustomKeyboardController.Create;
begin
  inherited Create;
  FHandlers := TList<IdxKeyboardHandlerService>.Create;
end;

destructor TdxCustomKeyboardController.Destroy;
begin
  FreeAndNil(FHandlers);
  inherited Destroy;
end;

procedure TdxCustomKeyboardController.AddHandler(const AHandler: IdxKeyboardHandlerService);
begin
  FHandlers.Add(AHandler);
end;

procedure TdxCustomKeyboardController.RemoveHandler(const AHandler: IdxKeyboardHandlerService);
begin
  FHandlers.Remove(AHandler);
end;

function TdxCustomKeyboardController.CanKeyDownModifyEdit(Key: Word; Shift: TShiftState): Boolean;
var
  Args: TdxKeyEventArgs;
  AHandler: IdxKeyboardHandlerService;
  I: Integer;
begin
  Result := False;
  Args := TdxKeyEventArgs.Create(Key, Shift);
  for I := FHandlers.Count - 1 downto 0 do
  begin
    AHandler := FHandlers[I];
    Result := AHandler.CanKeyDownModifyEdit(Args);
    if Result then
      Break;
  end;
end;

procedure TdxCustomKeyboardController.KeyDown(var Key: Word; Shift: TShiftState);
var
  Args: TdxKeyEventArgs;
  AHandler: IdxKeyboardHandlerService;
  I: Integer;
begin
  Args := TdxKeyEventArgs.Create(Key, Shift);
  for I := FHandlers.Count - 1 downto 0 do
  begin
    AHandler := FHandlers[I];
    Args.Handled := AHandler.HandleKeyDown(Args);
    if Args.Handled then
      Break;
  end;
  FSuppressKeyPressHandling := Args.Handled;
end;

procedure TdxCustomKeyboardController.KeyPress(var Key: Char);
var
  Args: TdxKeyPressEventArgs;
  AHandler: IdxKeyboardHandlerService;
  I: Integer;
begin
  if FSuppressKeyPressHandling then
    Exit;
  Args := TdxKeyPressEventArgs.Create(Key, []);
  for I := FHandlers.Count - 1 downto 0 do
  begin
    AHandler := FHandlers[I];
    Args.Handled := AHandler.HandleKeyPress(Args);
    if Args.Handled then
      Break;
  end;
end;

procedure TdxCustomKeyboardController.KeyUp(var Key: Word; Shift: TShiftState);
var
  Args: TdxKeyEventArgs;
  AHandler: IdxKeyboardHandlerService;
  I: Integer;
begin
  FSuppressKeyPressHandling := False;
  Args := TdxKeyEventArgs.Create(Key, Shift);
  for I := FHandlers.Count - 1 downto 0 do
  begin
    AHandler := FHandlers[I];
    Args.Handled := AHandler.HandleKeyUp(Args);
    if Args.Handled then
      Break;
  end;
end;

function TdxCustomKeyboardController.GetHandler: IdxKeyboardHandlerService;
begin
  if Handlers.Count > 0 then
    Result := Handlers.Last
  else
    Result := nil;
end;

end.
