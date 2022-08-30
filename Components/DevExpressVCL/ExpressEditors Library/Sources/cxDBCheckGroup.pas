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

unit cxDBCheckGroup;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Controls, Messages, cxCheckGroup, cxDBEdit, cxEdit;

type
  { TcxDBCheckGroupButton }

  TcxDBCheckGroupButton = class(TcxCheckGroupButton)
  private
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TcxDBCheckGroup }

  TcxDBCheckGroup = class(TcxCheckGroup)
  private
    function GetActiveProperties: TcxCheckGroupProperties;
    function GetDataBinding: TcxDBEditDataBinding;
    function GetProperties: TcxCheckGroupProperties;
    procedure SetDataBinding(const Value: TcxDBEditDataBinding);
    procedure SetProperties(Value: TcxCheckGroupProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    function GetButtonInstance: TWinControl; override;
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCheckGroupProperties read GetActiveProperties;
  published
    property Alignment;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Constraints;
    property DataBinding: TcxDBEditDataBinding read GetDataBinding
      write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditValue;
    property Enabled;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxCheckGroupProperties read GetProperties
      write SetProperties;
    property ShowHint;
    property StatesItems;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFocusChanged;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TcxDBCheckGroupButton }

constructor TcxDBCheckGroupButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
end;

procedure TcxDBCheckGroupButton.CMGetDataLink(var Message: TMessage);
begin
  CheckGroup.Perform(Message.Msg, Message.WParam, Message.LParam);
end;

{ TcxDBCheckGroup }

class function TcxDBCheckGroup.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckGroupProperties;
end;

function TcxDBCheckGroup.GetButtonInstance: TWinControl;
begin
  Result := TcxDBCheckGroupButton.Create(Self, IsInplace);
end;

class function TcxDBCheckGroup.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

function TcxDBCheckGroup.GetActiveProperties: TcxCheckGroupProperties;
begin
  Result := TcxCheckGroupProperties(InternalGetActiveProperties);
end;

function TcxDBCheckGroup.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

function TcxDBCheckGroup.GetProperties: TcxCheckGroupProperties;
begin
  Result := TcxCheckGroupProperties(inherited Properties);
end;

procedure TcxDBCheckGroup.SetDataBinding(const Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBCheckGroup.SetProperties(Value: TcxCheckGroupProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBCheckGroup.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

end.
