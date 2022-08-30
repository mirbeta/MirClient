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

unit dxDBCheckGroupBox;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Classes, Controls, Sysutils, cxDB, cxDBEdit, cxEdit,
  cxGroupBox, dxCheckGroupBox;

type
  { TdxDBCheckGroupBoxCheckBox }

  TdxDBCheckGroupBoxCheckBoxClass = class of TdxDBCheckGroupBoxCheckBox;

  TdxDBCheckGroupBoxCheckBox = class(TdxCustomCheckGroupBoxCheckBox)
  published
    property CheckAction;
    property Visible;
  end;

  { TdxDBCheckGroupBox }

  TdxDBCheckGroupBox = class(TdxCustomCheckGroupBox)
  private
    function GetDataBinding: TcxDBEditDataBinding;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetCheckBoxClass: TdxCustomCheckGroupBoxCheckBoxClass; override;
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  published
    property Alignment;
    property Anchors;
    property BiDiMode;
    property Caption;
    property CheckBox;
    property Constraints;
    property Ctl3D;
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Focusable;
    property PanelStyle;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property RedrawOnResize;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property UseDockManager;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawCaption;
    property OnCustomDrawContentBackground;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMeasureCaptionHeight;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

{ TdxDBCheckGroupBox }

class function TdxDBCheckGroupBox.GetCheckBoxClass: TdxCustomCheckGroupBoxCheckBoxClass;
begin
  Result := TdxDBCheckGroupBoxCheckBox;
end;

class function TdxDBCheckGroupBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

function TdxDBCheckGroupBox.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

procedure TdxDBCheckGroupBox.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TdxDBCheckGroupBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

end.
