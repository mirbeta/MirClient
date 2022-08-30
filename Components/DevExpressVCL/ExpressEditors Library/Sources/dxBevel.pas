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

unit dxBevel;

{$I cxVer.inc}

interface

uses
  Windows, Controls, Classes, Graphics, SysUtils, Forms, dxCore,
  cxLookAndFeels, cxLookAndFeelPainters, cxGraphics, cxControls, cxGeometry;

type

  { TdxCustomBevel }

  TdxCustomBevel = class(TGraphicControl,
    IcxLookAndFeelContainer,
    IdxScaleFactor,
    IdxSkinSupport)
  strict private
    FcxCanvas: TcxCanvas;
    FLookAndFeel: TcxLookAndFeel;
    FScaleFactor: TdxScaleFactor;
    FShape: TdxBevelShape;
    FStyle: TdxBevelStyle;

    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    procedure SetLookAndFeel(AValue: TcxLookAndFeel);
    procedure SetShape(AValue: TdxBevelShape);
    procedure SetStyle(AValue: TdxBevelStyle);
    // IdxScaleFactor
    function GetScaleFactor: TdxScaleFactor;
  protected
    function CanAutoHeight(AShape: TdxBevelShape): Boolean; virtual;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function CanAutoWidth(AShape: TdxBevelShape): Boolean; virtual;
  {$IFDEF DELPHIBERLIN}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  {$ELSE}
    procedure ChangeScale(M, D: Integer); override;
  {$ENDIF}
    function IsHorzLineShape(AShape: TdxBevelShape): Boolean;
    function IsVertLineShape(AShape: TdxBevelShape): Boolean;
    procedure LookAndFeelChangeHandler(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); virtual;
    procedure Paint; override;
    procedure SetParent(AValue: TWinControl); override;
    // IcxLookAndFeelContainer
    function GetLookAndFeel: TcxLookAndFeel; virtual;
    //
    property cxCanvas: TcxCanvas read FcxCanvas;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property Shape: TdxBevelShape read FShape write SetShape default dxbsBox;
    property Style: TdxBevelStyle read FStyle write SetStyle default dxbsLowered;
  end;

  { TdxBevel }

  TdxBevel = class(TdxCustomBevel)
  published
    property Align;
    property Anchors;
    property AutoSize default false;
    property Constraints;
    property LookAndFeel;
    property Shape;
    property Style;
    property Visible;
  end;

implementation

{ TdxCustomBevel }

constructor TdxCustomBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FLookAndFeel.OnChanged := LookAndFeelChangeHandler;
  FScaleFactor := TdxScaleFactor.Create;
  FcxCanvas := TcxCanvas.Create(Canvas);
  FShape := dxbsBox;
  Width := 50;
  Height := 50;
end;

destructor TdxCustomBevel.Destroy;
begin
  FreeAndNil(FcxCanvas);
  FreeAndNil(FLookAndFeel);
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

function TdxCustomBevel.CanAutoHeight(AShape: TdxBevelShape): Boolean;
begin
  Result := IsHorzLineShape(AShape) and (Align in [alTop, alBottom, alCustom, alNone]);
end;

function TdxCustomBevel.CanAutoWidth(AShape: TdxBevelShape): Boolean;
begin
  Result := IsVertLineShape(AShape) and (Align in [alLeft, alRight, alCustom, alNone]);
end;

function TdxCustomBevel.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if CanAutoWidth(Shape) then
    NewWidth := LookAndFeelPainter.GetBevelMinimalShapeSize(Shape).cx
  else
    if CanAutoHeight(Shape) then
      NewHeight := LookAndFeelPainter.GetBevelMinimalShapeSize(Shape).cy
    else
      Result := False;
end;

{$IFDEF DELPHIBERLIN}
procedure TdxCustomBevel.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ELSE}
procedure TdxCustomBevel.ChangeScale(M, D: Integer);
{$ENDIF}
begin
  ScaleFactor.Change(M, D);
  inherited;
  LookAndFeel.Refresh;
end;

function TdxCustomBevel.IsHorzLineShape(AShape: TdxBevelShape): Boolean;
begin
  Result := AShape in [dxbsLineTop, dxbsLineBottom, dxbsLineCenteredVert];
end;

function TdxCustomBevel.IsVertLineShape(AShape: TdxBevelShape): Boolean;
begin
  Result := AShape in [dxbsLineLeft, dxbsLineRight, dxbsLineCenteredHorz];
end;

procedure TdxCustomBevel.Paint;
begin
  if Shape <> dxbsNone then
    LookAndFeelPainter.DrawBevelShape(cxCanvas, ClientRect, Shape, Style)
  else
    if csDesigning in ComponentState then
      cxDrawHatchRect(cxCanvas, ClientRect, $EDC8A3);
end;

procedure TdxCustomBevel.SetParent(AValue: TWinControl);
begin
  if TcxControlHelper.CanSetParent(Self, AValue) then
  begin
    inherited SetParent(AValue);
    TcxControlHelper.UpdateScaleFactorOnParentChange(Self);
  end;
end;

function TdxCustomBevel.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := LookAndFeel.Painter;
end;

function TdxCustomBevel.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

procedure TdxCustomBevel.LookAndFeelChangeHandler(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  if not (csDestroying in (Application.ComponentState + ComponentState)) then
  begin
    if AutoSize then
      AdjustSize;
    Invalidate;
  end;
end;

procedure TdxCustomBevel.SetLookAndFeel(AValue: TcxLookAndFeel);
begin
  LookAndFeel.Assign(AValue);
end;

procedure TdxCustomBevel.SetShape(AValue: TdxBevelShape);
var
  APrevShape: TdxBevelShape;
begin
  if AValue <> FShape then
  begin
    APrevShape := Shape;
    FShape := AValue;
    if AutoSize then
    begin
      if CanAutoHeight(APrevShape) and CanAutoWidth(Shape) or CanAutoWidth(APrevShape) and CanAutoHeight(Shape) then
        SetBounds(Left, Top, Height, Width)
      else
        AdjustSize;
    end;
    Invalidate;
  end;
end;

procedure TdxCustomBevel.SetStyle(AValue: TdxBevelStyle);
begin
  if AValue <> FStyle then
  begin
    FStyle := AValue;
    Invalidate;
  end;
end;

function TdxCustomBevel.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

end.
