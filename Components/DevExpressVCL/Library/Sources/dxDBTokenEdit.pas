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

unit dxDBTokenEdit;

{$I cxVer.inc}

interface

uses
  Windows, Types, SysUtils, Classes, Graphics, Controls, Math, Variants, StdCtrls, Forms, Messages, ImgList,
  dxCore, dxCoreClasses, cxClasses, cxControls, cxGraphics, cxGeometry, dxTokenEdit, cxDBEdit, cxEdit;

type

  { TdxDBTokenEditDataBinding }

  TdxDBTokenEditDataBinding = class(TcxDBEditDataBinding)
  public
    procedure UpdateNotConnectedDBEditDisplayValue; override;
  end;

  { TdxDBTokenEdit }

  TdxDBTokenEdit = class(TdxCustomTokenEdit)
  strict private
    function GetDataBinding: TdxDBTokenEditDataBinding;
    procedure SetDataBinding(Value: TdxDBTokenEditDataBinding);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DataBinding: TdxDBTokenEditDataBinding read GetDataBinding write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
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

{ TdxDBTokenEditDataBinding }

procedure TdxDBTokenEditDataBinding.UpdateNotConnectedDBEditDisplayValue;
begin
  if not IsDataAvailable then
    Edit.EditValue := '';
end;

{ TdxDBTokenEdit }

class function TdxDBTokenEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TdxDBTokenEditDataBinding;
end;

function TdxDBTokenEdit.GetDataBinding: TdxDBTokenEditDataBinding;
begin
  Result := inherited DataBinding as TdxDBTokenEditDataBinding;
end;

procedure TdxDBTokenEdit.SetDataBinding(Value: TdxDBTokenEditDataBinding);
begin
  inherited DataBinding := Value;
end;

procedure TdxDBTokenEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

end.
