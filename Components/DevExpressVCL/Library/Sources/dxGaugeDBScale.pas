{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressGaugeControl                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSGAUGECONTROL AND ALL           }
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

unit dxGaugeDBScale;

{$I cxVer.inc}

interface

uses
  cxDB, cxDataUtils, dxGaugeCircularScale, dxGaugeDigitalScale, dxGaugeLinearScale;

type
  { TdxGaugeDBCircularScale }

  TdxGaugeDBCircularScale = class(TdxGaugeCustomCircularScale)
  private
    function GetDataBinding: TcxDBDataBinding;
    procedure SetDataBinding(const AValue: TcxDBDataBinding);
  protected
    class function GetScaleName: string; override;

    function GetDataBindingClass: TcxCustomDataBindingClass; override;
  public
    property Value: Single read GetValue;
  published
    property AnchorScaleIndex;
    property Captions;
    property DataBinding: TcxDBDataBinding read GetDataBinding write SetDataBinding;
    property OptionsAnimate;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeDBCircularHalfScale }

  TdxGaugeDBCircularHalfScale = class(TdxGaugeCustomCircularHalfScale)
  private
    function GetDataBinding: TcxDBDataBinding;
    procedure SetDataBinding(const AValue: TcxDBDataBinding);
  protected
    class function GetScaleName: string; override;

    function GetDataBindingClass: TcxCustomDataBindingClass; override;
  public
    property Value: Single read GetValue;
  published
    property AnchorScaleIndex;
    property Captions;
    property DataBinding: TcxDBDataBinding read GetDataBinding write SetDataBinding;
    property OptionsAnimate;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeDBCircularQuarterLeftScale }

  TdxGaugeDBCircularQuarterLeftScale = class(TdxGaugeCustomCircularQuarterLeftScale)
  private
    function GetDataBinding: TcxDBDataBinding;
    procedure SetDataBinding(const AValue: TcxDBDataBinding);
  protected
    class function GetScaleName: string; override;

    function GetDataBindingClass: TcxCustomDataBindingClass; override;
  public
    property Value: Single read GetValue;
  published
    property AnchorScaleIndex;
    property Captions;
    property DataBinding: TcxDBDataBinding read GetDataBinding write SetDataBinding;
    property OptionsAnimate;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeDBCircularQuarterRightScale }

  TdxGaugeDBCircularQuarterRightScale = class(TdxGaugeCustomCircularQuarterRightScale)
  private
    function GetDataBinding: TcxDBDataBinding;
    procedure SetDataBinding(const AValue: TcxDBDataBinding);
  protected
    class function GetScaleName: string; override;

    function GetDataBindingClass: TcxCustomDataBindingClass; override;
  public
    property Value: Single read GetValue;
  published
    property AnchorScaleIndex;
    property Captions;
    property DataBinding: TcxDBDataBinding read GetDataBinding write SetDataBinding;
    property OptionsAnimate;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeDBCircularThreeFourthScale }

  TdxGaugeDBCircularThreeFourthScale = class(TdxGaugeCustomCircularThreeFourthScale)
  private
    function GetDataBinding: TcxDBDataBinding;
    procedure SetDataBinding(const AValue: TcxDBDataBinding);
  protected
    class function GetScaleName: string; override;

    function GetDataBindingClass: TcxCustomDataBindingClass; override;
  public
    property Value: Single read GetValue;
  published
    property AnchorScaleIndex;
    property Captions;
    property DataBinding: TcxDBDataBinding read GetDataBinding write SetDataBinding;
    property OptionsAnimate;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeDBCircularWideScale }

  TdxGaugeDBCircularWideScale = class(TdxGaugeCustomCircularWideScale)
  private
    function GetDataBinding: TcxDBDataBinding;
    procedure SetDataBinding(const AValue: TcxDBDataBinding);
  protected
    class function GetScaleName: string; override;

    function GetDataBindingClass: TcxCustomDataBindingClass; override;
  public
    property Value: Single read GetValue;
  published
    property AnchorScaleIndex;
    property Captions;
    property DataBinding: TcxDBDataBinding read GetDataBinding write SetDataBinding;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeDBLinearScale }

  TdxGaugeDBLinearScale = class(TdxGaugeCustomLinearScale)
  private
    function GetDataBinding: TcxDBDataBinding;
    procedure SetDataBinding(const AValue: TcxDBDataBinding);
  protected
    class function GetScaleName: string; override;

    function GetDataBindingClass: TcxCustomDataBindingClass; override;
  public
    property Value: Single read GetValue;
  published
    property AnchorScaleIndex;
    property Captions;
    property DataBinding: TcxDBDataBinding read GetDataBinding write SetDataBinding;
    property OptionsAnimate;
    property OptionsLayout;
    property OptionsView;
    property Ranges;
    property StyleName;
    property Visible;

    property OnAnimate;
    property OnAnimationComplete;
  end;

  { TdxGaugeDBDigitalScale }

  TdxGaugeDBDigitalScale = class(TdxGaugeCustomDigitalScale)
  private
    function GetDataBinding: TcxDBDataBinding;
    procedure SetDataBinding(const AValue: TcxDBDataBinding);
  protected
    class function GetScaleName: string; override;

    function GetDataBindingClass: TcxCustomDataBindingClass; override;
  public
    property Value: string read GetValue;
  published
    property AnchorScaleIndex;
    property DataBinding: TcxDBDataBinding read GetDataBinding write SetDataBinding;
    property OptionsLayout;
    property OptionsView;
    property StyleName;
    property Visible;
  end;

implementation

uses
  dxGaugeCustomScale;

const
  sdxGaugeDBCustomScaleNamePrefix = 'DB ';

{ TdxGaugeDBCircularScale }

class function TdxGaugeDBCircularScale.GetScaleName: string;
begin
  Result := sdxGaugeDBCustomScaleNamePrefix + inherited GetScaleName;
end;

function TdxGaugeDBCircularScale.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxDBDataBinding;
end;

function TdxGaugeDBCircularScale.GetDataBinding: TcxDBDataBinding;
begin
  Result := inherited DataBinding as TcxDBDataBinding;
end;

procedure TdxGaugeDBCircularScale.SetDataBinding(const AValue: TcxDBDataBinding);
begin
  inherited DataBinding := AValue;
end;

{ TdxGaugeDBCircularHalfScale }

class function TdxGaugeDBCircularHalfScale.GetScaleName: string;
begin
  Result := sdxGaugeDBCustomScaleNamePrefix + inherited GetScaleName;
end;

function TdxGaugeDBCircularHalfScale.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxDBDataBinding;
end;

function TdxGaugeDBCircularHalfScale.GetDataBinding: TcxDBDataBinding;
begin
  Result := inherited DataBinding as TcxDBDataBinding;
end;

procedure TdxGaugeDBCircularHalfScale.SetDataBinding(const AValue: TcxDBDataBinding);
begin
  inherited DataBinding := AValue;
end;

{ TdxGaugeDBCircularQuarterLeftScale }

class function TdxGaugeDBCircularQuarterLeftScale.GetScaleName: string;
begin
  Result := sdxGaugeDBCustomScaleNamePrefix + inherited GetScaleName;
end;

function TdxGaugeDBCircularQuarterLeftScale.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxDBDataBinding;
end;

function TdxGaugeDBCircularQuarterLeftScale.GetDataBinding: TcxDBDataBinding;
begin
  Result := inherited DataBinding as TcxDBDataBinding;
end;

procedure TdxGaugeDBCircularQuarterLeftScale.SetDataBinding(const AValue: TcxDBDataBinding);
begin
  inherited DataBinding := AValue;
end;

{ TdxGaugeDBCircularQuarterRightScale }

class function TdxGaugeDBCircularQuarterRightScale.GetScaleName: string;
begin
  Result := sdxGaugeDBCustomScaleNamePrefix + inherited GetScaleName;
end;

function TdxGaugeDBCircularQuarterRightScale.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxDBDataBinding;
end;

function TdxGaugeDBCircularQuarterRightScale.GetDataBinding: TcxDBDataBinding;
begin
  Result := inherited DataBinding as TcxDBDataBinding;
end;

procedure TdxGaugeDBCircularQuarterRightScale.SetDataBinding(const AValue: TcxDBDataBinding);
begin
  inherited DataBinding := AValue;
end;

{ TdxGaugeDBCircularThreeFourthScale }

class function  TdxGaugeDBCircularThreeFourthScale.GetScaleName: string;
begin
  Result := sdxGaugeDBCustomScaleNamePrefix + inherited GetScaleName;
end;

function TdxGaugeDBCircularThreeFourthScale.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxDBDataBinding;
end;

function TdxGaugeDBCircularThreeFourthScale.GetDataBinding: TcxDBDataBinding;
begin
  Result := inherited DataBinding as TcxDBDataBinding;
end;

procedure TdxGaugeDBCircularThreeFourthScale.SetDataBinding(const AValue: TcxDBDataBinding);
begin
  inherited DataBinding := AValue;
end;

{ TdxGaugeDBCircularWideScale }

class function  TdxGaugeDBCircularWideScale.GetScaleName: string;
begin
  Result := sdxGaugeDBCustomScaleNamePrefix + inherited GetScaleName;
end;

function TdxGaugeDBCircularWideScale.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxDBDataBinding;
end;

function TdxGaugeDBCircularWideScale.GetDataBinding: TcxDBDataBinding;
begin
  Result := inherited DataBinding as TcxDBDataBinding;
end;

procedure TdxGaugeDBCircularWideScale.SetDataBinding(const AValue: TcxDBDataBinding);
begin
  inherited DataBinding := AValue;
end;

{ TdxGaugeDBLinearScale }

class function TdxGaugeDBLinearScale.GetScaleName: string;
begin
  Result := sdxGaugeDBCustomScaleNamePrefix + inherited GetScaleName;
end;

function TdxGaugeDBLinearScale.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxDBDataBinding;
end;

function TdxGaugeDBLinearScale.GetDataBinding: TcxDBDataBinding;
begin
  Result := inherited DataBinding as TcxDBDataBinding;
end;

procedure TdxGaugeDBLinearScale.SetDataBinding(const AValue: TcxDBDataBinding);
begin
  inherited DataBinding := AValue;
end;

{ TdxGaugeDBDigitalScale }

class function TdxGaugeDBDigitalScale.GetScaleName: string;
begin
  Result := sdxGaugeDBCustomScaleNamePrefix + inherited GetScaleName;
end;

function TdxGaugeDBDigitalScale.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxDBDataBinding;
end;

function TdxGaugeDBDigitalScale.GetDataBinding: TcxDBDataBinding;
begin
  Result := inherited DataBinding as TcxDBDataBinding;
end;

procedure TdxGaugeDBDigitalScale.SetDataBinding(const AValue: TcxDBDataBinding);
begin
  inherited DataBinding := AValue;
end;

initialization
  dxGaugeRegisterScale(TdxGaugeDBCircularScale, False);
  dxGaugeRegisterScale(TdxGaugeDBCircularHalfScale, False);
  dxGaugeRegisterScale(TdxGaugeDBCircularQuarterLeftScale, False);
  dxGaugeRegisterScale(TdxGaugeDBCircularQuarterRightScale, False);
  dxGaugeRegisterScale(TdxGaugeDBCircularThreeFourthScale, False);
  dxGaugeRegisterScale(TdxGaugeDBCircularWideScale, False);
  dxGaugeRegisterScale(TdxGaugeDBDigitalScale, False);
  dxGaugeRegisterScale(TdxGaugeDBLinearScale, False);

finalization
  dxGaugeUnregisterScale(TdxGaugeDBLinearScale, False);
  dxGaugeUnregisterScale(TdxGaugeDBDigitalScale, False);
  dxGaugeUnregisterScale(TdxGaugeDBCircularWideScale, False);
  dxGaugeUnregisterScale(TdxGaugeDBCircularThreeFourthScale, False);
  dxGaugeUnregisterScale(TdxGaugeDBCircularQuarterRightScale, False);
  dxGaugeUnregisterScale(TdxGaugeDBCircularQuarterLeftScale, False);
  dxGaugeUnregisterScale(TdxGaugeDBCircularHalfScale, False);
  dxGaugeUnregisterScale(TdxGaugeDBCircularScale, False);

end.

