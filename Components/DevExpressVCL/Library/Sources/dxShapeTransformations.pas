{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library graphics classes          }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit dxShapeTransformations;

interface

{$I cxVer.inc}

uses
  Classes, cxGeometry,
  dxShapePrimitives;

type
  { TdxShapeTranslateTransformation }

  TdxShapeTranslateTransformation = class(TdxShapeCustomTransformation)
  private
    FOffset: TdxPointF;

    procedure SetOffsetX(const AValue: Single);
    procedure SetOffsetY(const AValue: Single);
  public
    constructor Create; override;

    procedure Assign(ASource: TPersistent); override;
    class function GetName: string; override;
  published
    property X: Single read FOffset.X write SetOffsetX;
    property Y: Single read FOffset.Y write SetOffsetY;
  end;

  { TdxShapeCenteredTransformation }

  TdxShapeCenterTransformation = class(TdxShapeCustomTransformation)
  private
    FCenter: TdxPointF;

    procedure SetCenterX(const AValue: Single);
    procedure SetCenterY(const AValue: Single);
  published
    property CenterX: Single read FCenter.X write SetCenterX;
    property CenterY: Single read FCenter.Y write SetCenterY;
  end;

  { TdxShapeScaleTransformation }

  TdxShapeScaleTransformation = class(TdxShapeCenterTransformation)
  private
    FScale: TdxPointF;

    procedure SetScaleX(const AValue: Single);
    procedure SetScaleY(const AValue: Single);
  public
    procedure Assign(ASource: TPersistent); override;
    class function GetName: string; override;
  published
    property ScaleX: Single read FScale.X write SetScaleX;
    property ScaleY: Single read FScale.Y write SetScaleY;
  end;

  { TdxShapeSkewTransformation }

  TdxShapeSkewTransformation = class(TdxShapeCenterTransformation)
  private
    FAngle: TdxPointF;

    procedure SetAngleX(const AValue: Single);
    procedure SetAngleY(const AValue: Single);
  public
    constructor Create; override;

    procedure Assign(ASource: TPersistent); override;
    class function GetName: string; override;
  published
    property AngleX: Single read FAngle.X write SetAngleX;
    property AngleY: Single read FAngle.Y write SetAngleY;
  end;

  { TdxShapeRotateTransformation }

  TdxShapeRotateTransformation = class(TdxShapeCenterTransformation)
  private
    FAngle: Single;

    procedure SetAngle(const AValue: Single);
  public
    constructor Create; override;

    procedure Assign(ASource: TPersistent); override;
    class function GetName: string; override;
  published
    property Angle: Single read FAngle write SetAngle;
  end;

implementation

uses
  dxShapeReaders;

{ TdxShapeTranslateTransformation }

constructor TdxShapeTranslateTransformation.Create;
begin
  inherited;
  FType := dxsttTranslate;
end;

procedure TdxShapeTranslateTransformation.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TdxShapeTranslateTransformation then
  begin
    BeginUpdate;
    X := TdxShapeTranslateTransformation(ASource).X;
    Y := TdxShapeTranslateTransformation(ASource).Y;
    EndUpdate;
  end;
end;

class function TdxShapeTranslateTransformation.GetName: string;
begin
  Result := 'TranslateTransform';
end;

procedure TdxShapeTranslateTransformation.SetOffsetX(const AValue: Single);
begin
  if FOffset.X <> AValue then
  begin
    FOffset.X := AValue;
    Changed;
  end;
end;

procedure TdxShapeTranslateTransformation.SetOffsetY(const AValue: Single);
begin
  if FOffset.Y <> AValue then
  begin
    FOffset.Y := AValue;
    Changed;
  end;
end;

{ TdxShapeCenterTransformation }

procedure TdxShapeCenterTransformation.SetCenterX(const AValue: Single);
begin
  if FCenter.X <> AValue then
  begin
    FCenter.X := AValue;
    Changed;
  end;
end;

procedure TdxShapeCenterTransformation.SetCenterY(const AValue: Single);
begin
  if FCenter.Y <> AValue then
  begin
    FCenter.Y := AValue;
    Changed;
  end;
end;

{ TdxShapeScaleTransformation }

procedure TdxShapeScaleTransformation.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TdxShapeScaleTransformation then
  begin
    BeginUpdate;
    ScaleX := TdxShapeScaleTransformation(ASource).ScaleX;
    ScaleY := TdxShapeScaleTransformation(ASource).ScaleY;
    EndUpdate;
  end;
end;

class function TdxShapeScaleTransformation.GetName: string;
begin
  Result := 'ScaleTransform';
end;

procedure TdxShapeScaleTransformation.SetScaleX(const AValue: Single);
begin
  if FScale.X <> AValue then
  begin
    FScale.X := AValue;
    Changed;
  end;
end;

procedure TdxShapeScaleTransformation.SetScaleY(const AValue: Single);
begin
  if FScale.Y <> AValue then
  begin
    FScale.Y := AValue;
    Changed;
  end;
end;

{ TdxShapeSkewTransformation }

constructor TdxShapeSkewTransformation.Create;
begin
  inherited;
  FType := dxsttSkew;
end;

procedure TdxShapeSkewTransformation.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TdxShapeSkewTransformation then
  begin
    BeginUpdate;
    AngleX := TdxShapeSkewTransformation(ASource).AngleX;
    AngleY := TdxShapeSkewTransformation(ASource).AngleY;
    EndUpdate;
  end;
end;

class function TdxShapeSkewTransformation.GetName: string;
begin
  Result := 'SkewTransform';
end;

procedure TdxShapeSkewTransformation.SetAngleX(const AValue: Single);
begin
  if FAngle.X <> AValue then
  begin
    FAngle.X := AValue;
    Changed;
  end;
end;

procedure TdxShapeSkewTransformation.SetAngleY(const AValue: Single);
begin
  if FAngle.Y <> AValue then
  begin
    FAngle.Y := AValue;
    Changed;
  end;
end;

{ TdxShapeRotateTransformation }

procedure TdxShapeRotateTransformation.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TdxShapeRotateTransformation then
    Angle := TdxShapeRotateTransformation(ASource).Angle;
end;

class function TdxShapeRotateTransformation.GetName: string;
begin
  Result := 'RotateTransform';
end;

constructor TdxShapeRotateTransformation.Create;
begin
  inherited;
  FType := dxsttRotate;
end;

procedure TdxShapeRotateTransformation.SetAngle(const AValue: Single);
begin
  if FAngle <> AValue then
  begin
    FAngle := AValue;
    Changed;
  end;
end;

end.
