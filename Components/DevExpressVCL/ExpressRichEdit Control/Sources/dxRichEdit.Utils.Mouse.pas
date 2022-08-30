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

unit dxRichEdit.Utils.Mouse;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Windows, SysUtils, Classes, Controls,
  ActiveX;

type
  { TdxMouseEventArgs }

  TdxMouseButtons = set of TMouseButton;

  TdxMouseEventArgs = record
  strict private
    constructor Create(AButtons: TdxMouseButtons; AShift: TShiftState;
      const AMousePos: TPoint; ADelta: Integer; AHorizontal: Boolean); overload;
  public
    Buttons: TdxMouseButtons;
    Shift: TShiftState;
    MousePos: TPoint;
    WheelDelta: Integer;
    Horizontal: Boolean;
    constructor Create(AButtons: TdxMouseButtons; AShift: TShiftState; const AMousePos: TPoint); overload;
    constructor Create(AShift: TShiftState; const AMousePos: TPoint; ADelta: Integer; AHorizontal: Boolean = False); overload;
  end;

  TdxMouseEventArgsArray = array of TdxMouseEventArgs;

  { Drag and Drop}

  TdxDragDropEffect = (None, Copy, Link, Move, Scroll);
  TdxDragDropEffects = set of TdxDragDropEffect;

  { TdxDragEventArgs }

  PdxDragEventArgs = ^TdxDragEventArgs;
  TdxDragEventArgs = record
  private
    function GetP: TPoint;
    procedure SetP(const P: TPoint);
  public
    AllowedEffect: TdxDragDropEffects;
    Data: IDataObject;
    Effect: TdxDragDropEffects;
    KeyState: TShiftState;
    X: Longint;
    Y: Longint;
    property P: TPoint read GetP write SetP;
  end;

  { TdxDragEventArgsHelper }

  TdxDragEventArgsHelper = record helper for TdxDragEventArgs
    class function CreateFormOle(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): TdxDragEventArgs; static;
  end;

  TdxDragAction = (Cancel, Continue, Drop);

  { TdxQueryContinueDragEventArgs }

  TdxQueryContinueDragEventArgs = record
    Action: TdxDragAction;
    EscapePressed: Boolean;
    KeyStates: TShiftState;
    constructor Create(AAction: TdxDragAction; AEscapePressed: Boolean; AKeyStates: TShiftState); overload;
    constructor Create(fEscapePressed: BOOL; grfKeyState: Longint); overload;
  end;
  PdxQueryContinueDragEventArgs = ^TdxQueryContinueDragEventArgs;

  TdxGiveFeedbackEventArgs = record
    Effect: TdxDragDropEffects;
    UseDefaultCursors: Boolean;
    constructor Create(AEffect: TdxDragDropEffects; AUseDefaultCursors: Boolean); overload;
    constructor Create(dwEffect: Longint); overload;
  end;
  PdxGiveFeedbackEventArgs = ^TdxGiveFeedbackEventArgs;

  { IdxCustomMouseState }

  IdxCustomMouseState = interface
  ['{119E3746-78CD-4F20-9502-7B6B3FD6E405}']
    procedure HandleLongMouseDown;
    procedure HandleMouseDown(const Args: TdxMouseEventArgs);
    procedure HandleMouseDoubleClick(const Args: TdxMouseEventArgs);
    procedure HandleMouseTripleClick(const Args: TdxMouseEventArgs);
    procedure HandleMouseMove(const Args: TdxMouseEventArgs);
    procedure HandleMouseUp(const Args: TdxMouseEventArgs);
    procedure HandleMouseWheel(const Args: TdxMouseEventArgs);

    procedure DoCancelMode;
    procedure DoDragEnter(var Args: TdxDragEventArgs);
    procedure DoDragOver(var Args: TdxDragEventArgs);
    procedure DoDragDrop(var Args: TdxDragEventArgs);
    procedure DoDragLeave;
    procedure GiveFeedback(Args: PdxGiveFeedbackEventArgs);
    procedure QueryContinueDrag(Args: PdxQueryContinueDragEventArgs);
  end;

  { TdxCustomMouseController }

  TdxCustomMouseController = class abstract
  public
    procedure SwitchStateCore(const ANewState: IdxCustomMouseState; const AMousePosition: TPoint); virtual; abstract;

    function MouseActivate(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; HitTest: Integer): TMouseActivate; virtual; abstract;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual; abstract;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual; abstract;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual; abstract;
    function MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; virtual; abstract;

    procedure DoCancelMode; virtual; abstract;
    function DoContextPopup(var Args: TdxMouseEventArgs): Boolean; virtual; abstract;

    procedure DoDragEnter(var Args: TdxDragEventArgs); virtual; abstract;
    procedure DoDragOver(var Args: TdxDragEventArgs); virtual; abstract;
    procedure DoDragDrop(var Args: TdxDragEventArgs); virtual; abstract;
    procedure DoDragLeave; virtual; abstract;
    procedure GiveFeedback(Args: PdxGiveFeedbackEventArgs); virtual; abstract;
    procedure QueryContinueDrag(Args: PdxQueryContinueDragEventArgs); virtual; abstract;
 end;

  { IdxHotZoneVisitor }

  IdxHotZoneVisitor = interface
  end;

  { IdxHotZone }

  IdxHotZone = interface
    function HitTest(const P: TPoint; ADpi, AScaleFactor: Single): Boolean;
    procedure Accept(const AVisitor: IdxHotZoneVisitor);
  end;

  { IdxGestureStateIndicator }

  IdxGestureStateIndicator = interface
  ['{EA548B8F-0ABA-4D5A-8DE8-188C19694303}']
    function GetGestureActivated: Boolean;
    procedure OnGestureBegin;
    procedure OnGestureEnd;
    property GestureActivated: Boolean read GetGestureActivated;
  end;

function dxDragDropEffectsToOleDragDropEffects(const Value: TdxDragDropEffects): LongWord;
function dxOleDragDropEffectsToDragDropEffects(const Value: LongWord): TdxDragDropEffects;

implementation

uses
  Forms;

function dxDragDropEffectsToOleDragDropEffects(const Value: TdxDragDropEffects): LongWord;
begin
  Result := DROPEFFECT_NONE;
  if TdxDragDropEffect.Copy in Value then
    Result := Result or DROPEFFECT_COPY;
  if TdxDragDropEffect.Link in Value then
    Result := Result or DROPEFFECT_LINK;
  if TdxDragDropEffect.Move in Value then
    Result := Result or DROPEFFECT_MOVE;
  if TdxDragDropEffect.Scroll in Value then
    Result := Result or DROPEFFECT_SCROLL;
end;

function dxOleDragDropEffectsToDragDropEffects(const Value: LongWord): TdxDragDropEffects;
begin
  Result := [];
  if Value and DROPEFFECT_COPY = DROPEFFECT_COPY then
    Include(Result, TdxDragDropEffect.Copy);
  if Value and DROPEFFECT_LINK = DROPEFFECT_LINK then
    Include(Result, TdxDragDropEffect.Link);
  if Value and DROPEFFECT_MOVE = DROPEFFECT_MOVE then
    Include(Result, TdxDragDropEffect.Move);
  if Value and DROPEFFECT_SCROLL = DROPEFFECT_SCROLL then
    Include(Result, TdxDragDropEffect.Scroll);
end;

{ TdxMouseEventArgs }

constructor TdxMouseEventArgs.Create(AButtons: TdxMouseButtons; AShift: TShiftState; const AMousePos: TPoint;
  ADelta: Integer; AHorizontal: Boolean);
begin
  Buttons := AButtons;
  Shift := AShift;
  MousePos := AMousePos;
  WheelDelta := ADelta;
  Horizontal := AHorizontal;
end;

constructor TdxMouseEventArgs.Create(AButtons: TdxMouseButtons;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  Create(AButtons, AShift, AMousePos, 0, False);
end;

constructor TdxMouseEventArgs.Create(AShift: TShiftState;
  const AMousePos: TPoint; ADelta: Integer; AHorizontal: Boolean);
begin
  Create([], AShift, AMousePos, ADelta, AHorizontal);
end;

{ TdxDragEventArgs }

function TdxDragEventArgs.GetP: TPoint;
begin
  Result := Point(X, Y);
end;

procedure TdxDragEventArgs.SetP(const P: TPoint);
begin
  X := P.X;
  Y := P.Y;
end;

{ TdxDragEventArgsHelper }

class function TdxDragEventArgsHelper.CreateFormOle(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): TdxDragEventArgs;
begin
  Result.Data := dataObj;
  Result.KeyState := KeyDataToShiftState(grfKeyState);
  Result.AllowedEffect := dxOleDragDropEffectsToDragDropEffects(dwEffect);
  Result.Effect := [];
  Result.P := pt;
end;

{ TdxQueryContinueDragEventArgs }

constructor TdxQueryContinueDragEventArgs.Create(AAction: TdxDragAction;
  AEscapePressed: Boolean; AKeyStates: TShiftState);
begin
  Action := AAction;
  EscapePressed := AEscapePressed;
  KeyStates := AKeyStates;
end;

constructor TdxQueryContinueDragEventArgs.Create(fEscapePressed: BOOL;
  grfKeyState: Integer);
begin
  KeyStates := KeysToShiftState(grfKeyState);
  Action := TdxDragAction.Continue;
  EscapePressed := fEscapePressed;
  if EscapePressed then
    Action := TdxDragAction.Cancel
  else
    if [ssLeft, ssRight] * KeyStates = [] then
      Action := TdxDragAction.Drop;
end;

{ TdxGiveFeedbackEventArgs }

constructor TdxGiveFeedbackEventArgs.Create(AEffect: TdxDragDropEffects;
  AUseDefaultCursors: Boolean);
begin

end;

constructor TdxGiveFeedbackEventArgs.Create(dwEffect: Integer);
begin
  Effect := dxOleDragDropEffectsToDragDropEffects(dwEffect);
  UseDefaultCursors := True;
end;

end.
