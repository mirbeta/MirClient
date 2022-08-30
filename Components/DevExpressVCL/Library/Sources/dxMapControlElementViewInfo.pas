{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxMapControlElementViewInfo;

interface

{$I cxVer.inc}

uses
  SysUtils, Graphics, Classes, Types, RTLConsts, Forms, Math, Windows, Controls,
  dxCoreClasses, cxClasses, cxControls,  cxGeometry, cxGraphics, dxGdiPlusClasses,
  cxLookAndFeelPainters, dxCustomHint, dxScreenTip;

type
  TdxMapControlEnumElementStateProc = procedure(AState: TdxMapControlElementState) of object;

  TdxMapControlElementViewInfo = class(TcxIUnknownObject, IcxHintableObject)
  private
    FBounds: TRect;
   // FPainter: TdxMapControlPainter;
    FSize: TSize;
    FState: TdxMapControlElementState;
  //  FViewInfo: TdxMapControlViewInfo;
    FVisibleElements: TdxFastObjectList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TdxMapControlElementViewInfo;
  //  function GetController: TdxMapControlController;
    procedure SetState(Value: TdxMapControlElementState);
  protected
    procedure Add(AElement: TdxMapControlElementViewInfo);
    procedure AddVisibleElements; virtual;
    procedure Insert(AIndex: Integer; AElement: TdxMapControlElementViewInfo);
    function DoCalculateSize: TSize; virtual;
    procedure DoElementDestroying; virtual;
    function GetForbiddenStates: TdxMapControlElementStates; virtual;
    function GetHitTestIndex: Integer; virtual;
    function GetIsVisible: Boolean; virtual;
    function GetState: TdxMapControlElementState; virtual;
    procedure InitializeVisibleElements; virtual;
    procedure Invalidate; virtual;
    function IsCapture: Boolean; virtual;
    function IsEnoughSpace(const AAvailableBounds: TRect): Boolean;
    function PtInElement(const APoint: TPoint): Boolean; virtual;
    procedure Remove(AElement: TdxMapControlElementViewInfo);
    procedure SetHitTest(AHitTest: TObject); virtual;

    // IcxHintableObject
    function HasHintPoint(const P: TPoint): Boolean;
    function IsHintAtMousePos: Boolean;
    function UseHintHidePause: Boolean;

    property Size: TSize read FSize;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CalculateBounds; virtual;
    procedure CalculateSize; virtual;
    procedure ClearCache; virtual;
    procedure Click; virtual;
    function GetHint: string; virtual;
    function GetHitTest(AHitTest: TObject): Boolean; virtual;
    function GetScreenTip: TdxScreenTip; virtual;
    procedure Initialize; virtual;
    function IsElementVisible(AElement: TdxMapControlElementViewInfo): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure Paint(ACanvas: TcxCanvas); virtual;

    property Bounds: TRect read FBounds write FBounds;
    property Count: Integer read GetCount;
  //  property Controller: TdxMapControlController read GetController;
    property IsVisible: Boolean read GetIsVisible;
    property Items[Index: Integer]: TdxMapControlElementViewInfo read GetItems; default;
  //  property Painter: TdxMapControlPainter read FPainter;
    property State: TdxMapControlElementState read GetState write SetState;
  //  property ViewInfo: TdxMapControlViewInfo read FViewInfo;
  end;

implementation

uses
  dxMapControlViewInfo;

{ TdxMapControlElementViewInfo }

constructor TdxMapControlElementViewInfo.Create;
begin
  inherited Create;
//  FViewInfo := AViewInfo;
//  FPainter := AViewInfo.Painter;
  FVisibleElements := TdxFastObjectList.Create(False);
end;

destructor TdxMapControlElementViewInfo.Destroy;
begin
  DoElementDestroying;
  FreeAndNil(FVisibleElements);
  inherited Destroy;
end;

procedure TdxMapControlElementViewInfo.CalculateBounds;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TdxMapControlElementViewInfo).CalculateBounds;
end;

procedure TdxMapControlElementViewInfo.CalculateSize;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TdxMapControlElementViewInfo).CalculateSize;
  FSize := DoCalculateSize;
end;

procedure TdxMapControlElementViewInfo.ClearCache;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TdxMapControlElementViewInfo).ClearCache;
end;

procedure TdxMapControlElementViewInfo.Click;
begin
end;

function TdxMapControlElementViewInfo.GetHint: string;
begin
  Result := '';
end;

function TdxMapControlElementViewInfo.GetCount: Integer;
begin
  Result := FVisibleElements.Count;
end;

function TdxMapControlElementViewInfo.GetItems(
  Index: Integer): TdxMapControlElementViewInfo;
begin
  Result := FVisibleElements[Index] as TdxMapControlElementViewInfo;
end;

function TdxMapControlElementViewInfo.GetHitTest(AHitTest: TObject): Boolean;
var
  I: Integer;
begin
  Result := (GetHitTestIndex <> mchtNone) and PtInElement((AHitTest as TdxMapControlHitTest).HitPoint);
  if Result then
    SetHitTest(AHitTest);
  for I := FVisibleElements.Count - 1 downto 0 do
    if (FVisibleElements[I] as TdxMapControlElementViewInfo).GetHitTest(AHitTest) then
    begin
      Result := True;
      Break;
    end;
end;

function TdxMapControlElementViewInfo.GetScreenTip: TdxScreenTip;
begin
  Result := nil;
end;

procedure TdxMapControlElementViewInfo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TdxMapControlElementViewInfo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TdxMapControlElementViewInfo.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TdxMapControlElementViewInfo.Paint(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TdxMapControlElementViewInfo).Paint(ACanvas);
end;

//function TdxMapControlElementViewInfo.GetController: TdxMapControlController;
//begin
//  Result := FViewInfo.Controller;
//end;

procedure TdxMapControlElementViewInfo.Add(
  AElement: TdxMapControlElementViewInfo);
begin
  FVisibleElements.Add(AElement)
end;

procedure TdxMapControlElementViewInfo.AddVisibleElements;
begin
end;

procedure TdxMapControlElementViewInfo.Insert(AIndex: Integer;
  AElement: TdxMapControlElementViewInfo);
begin
  FVisibleElements.Insert(AIndex, AElement);
end;

function TdxMapControlElementViewInfo.DoCalculateSize: TSize;
begin
  Result := cxNullSize;
end;

procedure TdxMapControlElementViewInfo.DoElementDestroying;
begin
end;

function TdxMapControlElementViewInfo.GetForbiddenStates: TdxMapControlElementStates;
begin
  Result := [];
end;

function TdxMapControlElementViewInfo.GetHitTestIndex: Integer;
begin
  Result := mchtNone;
end;

function TdxMapControlElementViewInfo.GetIsVisible: Boolean;
begin
  Result := True;
end;

function TdxMapControlElementViewInfo.GetState: TdxMapControlElementState;
begin
  Result := FState;
end;

procedure TdxMapControlElementViewInfo.Initialize;
begin
  FVisibleElements.Clear;
  AddVisibleElements;
  InitializeVisibleElements;
end;

procedure TdxMapControlElementViewInfo.Invalidate;
begin
end;

function TdxMapControlElementViewInfo.IsCapture: Boolean;
begin
  Result := False;
end;

procedure TdxMapControlElementViewInfo.InitializeVisibleElements;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TdxMapControlElementViewInfo).Initialize;
end;

function TdxMapControlElementViewInfo.IsElementVisible(AElement: TdxMapControlElementViewInfo): Boolean;
var
  I: Integer;
begin
  Result := FVisibleElements.IndexOf(AElement) <> -1;
  if not Result then
    for I := 0 to FVisibleElements.Count - 1 do
    begin
      Result := (FVisibleElements[I] as TdxMapControlElementViewInfo).IsElementVisible(AElement);
      if Result then
        Break;
    end;
end;

function TdxMapControlElementViewInfo.IsEnoughSpace(const AAvailableBounds: TRect): Boolean;
begin
  Result := (cxRectSize(AAvailableBounds).cx > Size.cx) and
    (cxRectSize(AAvailableBounds).cy > Size.cy);
end;

function TdxMapControlElementViewInfo.PtInElement(const APoint: TPoint): Boolean;
begin
  Result := cxRectPtIn(Bounds, APoint);
end;

procedure TdxMapControlElementViewInfo.Remove(
  AElement: TdxMapControlElementViewInfo);
begin
  FVisibleElements.Remove(AElement);
end;

procedure TdxMapControlElementViewInfo.SetHitTest(AHitTest: TObject);
var
  AMapControlHitTest: TdxMapControlHitTest;
begin
  AMapControlHitTest := AHitTest as TdxMapControlHitTest;
  AMapControlHitTest.SetBitState(GetHitTestIndex, True);
  AMapControlHitTest.HitObject := Self;
end;

function TdxMapControlElementViewInfo.HasHintPoint(const P: TPoint): Boolean;
begin
  Result := PtInElement(P);
end;

function TdxMapControlElementViewInfo.IsHintAtMousePos: Boolean;
begin
  Result := False;
end;

function TdxMapControlElementViewInfo.UseHintHidePause: Boolean;
begin
  Result := True;
end;

procedure TdxMapControlElementViewInfo.SetState(
  Value: TdxMapControlElementState);
begin
  if not (Value in GetForbiddenStates) and (FState <> Value) then
  begin
    FState := Value;
    Invalidate;
  end;
end;

end.
