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

unit dxDBDateTimeWheelPicker;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Classes, Controls, Dialogs, Forms, Graphics, Messages,
  StdCtrls, SysUtils, cxDBEdit, cxEdit, dxDateTimeWheelPicker;

type

  { TdxDBDateTimeWheelPicker }

  TdxDBDateTimeWheelPicker = class(TdxCustomDateTimeWheelPicker)
  private
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    function GetActiveProperties: TdxDateTimeWheelPickerProperties;
    function GetDataBinding: TcxDBEditDataBinding;
    function GetProperties: TdxDateTimeWheelPickerProperties;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure SetProperties(Value: TdxDateTimeWheelPickerProperties);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property ActiveProperties: TdxDateTimeWheelPickerProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TdxDateTimeWheelPickerProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnEditing;
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
  end;

implementation

{ TdxDBDateTimeWheelPicker }

class function TdxDBDateTimeWheelPicker.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxDateTimeWheelPickerProperties;
end;

class function TdxDBDateTimeWheelPicker.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

procedure TdxDBDateTimeWheelPicker.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

function TdxDBDateTimeWheelPicker.GetActiveProperties: TdxDateTimeWheelPickerProperties;
begin
  Result := TdxDateTimeWheelPickerProperties(InternalGetActiveProperties);
end;

function TdxDBDateTimeWheelPicker.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

function TdxDBDateTimeWheelPicker.GetProperties: TdxDateTimeWheelPickerProperties;
begin
  Result := TdxDateTimeWheelPickerProperties(inherited Properties);
end;

procedure TdxDBDateTimeWheelPicker.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TdxDBDateTimeWheelPicker.SetProperties(Value: TdxDateTimeWheelPickerProperties);
begin
  Properties.Assign(Value);
end;

end.
