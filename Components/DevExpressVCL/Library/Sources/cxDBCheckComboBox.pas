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

unit cxDBCheckComboBox;

interface

{$I cxVer.inc}

uses
  Variants, Windows, Classes, Controls, Dialogs, Forms, Graphics, Messages,
  StdCtrls, SysUtils, cxCheckComboBox, cxControls, cxDBEdit, cxEdit;

type
  { TcxDBCheckComboBox }

  TcxDBCheckComboBox = class(TcxCustomCheckComboBox)
  private
    function GetActiveProperties: TcxCheckComboBoxProperties;
    function GetDataBinding: TcxDBEditDataBinding;
    function GetProperties: TcxCheckComboBoxProperties;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure SetProperties(Value: TcxCheckComboBoxProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCheckComboBoxProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxCheckComboBoxProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
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
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TcxDBCheckComboBox }

class function TcxDBCheckComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckComboBoxProperties;
end;

class function TcxDBCheckComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

function TcxDBCheckComboBox.GetActiveProperties: TcxCheckComboBoxProperties;
begin
  Result := TcxCheckComboBoxProperties(InternalGetActiveProperties);
end;

function TcxDBCheckComboBox.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBCheckComboBox.GetProperties: TcxCheckComboBoxProperties;
begin
  Result := TcxCheckComboBoxProperties(inherited Properties);
end;

procedure TcxDBCheckComboBox.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBCheckComboBox.SetProperties(Value: TcxCheckComboBoxProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBCheckComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

end.
