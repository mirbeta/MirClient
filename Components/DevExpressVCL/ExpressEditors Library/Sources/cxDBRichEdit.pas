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

unit cxDBRichEdit;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Controls, Graphics, Messages, cxDBEdit, cxEdit, cxRichEdit;

type
   TcxDBRichEditDataBinding = class(TcxDBTextEditDataBinding)
   protected
     procedure UpdateData; override;
   end;

  TcxDBRichEdit = class(TcxCustomRichEdit)
  private
    function GetActiveProperties: TcxRichEditProperties;
    function GetDataBinding: TcxDBRichEditDataBinding;
    function GetProperties: TcxRichEditProperties;
    procedure SetDataBinding(Value: TcxDBRichEditDataBinding);
    procedure SetProperties(Value: TcxRichEditProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure EditingChanged; override;
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    function RealReadOnly: Boolean; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxRichEditProperties read GetActiveProperties;
  published
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBRichEditDataBinding read GetDataBinding
      write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxRichEditProperties read GetProperties
      write SetProperties;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  dxCore;

{ TcxDBRichEdit }

class function TcxDBRichEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxRichEditProperties;
end;

procedure TcxDBRichEdit.EditingChanged;
begin
  UpdateInnerEditReadOnly;
end;

class function TcxDBRichEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBRichEditDataBinding;
end;

function TcxDBRichEdit.RealReadOnly: Boolean;
begin
  Result := inherited RealReadOnly or not DataBinding.Editing;
end;

function TcxDBRichEdit.GetActiveProperties: TcxRichEditProperties;
begin
  Result := TcxRichEditProperties(InternalGetActiveProperties);
end;

function TcxDBRichEdit.GetDataBinding: TcxDBRichEditDataBinding;
begin
  Result := TcxDBRichEditDataBinding(FDataBinding);
end;

function TcxDBRichEdit.GetProperties: TcxRichEditProperties;
begin
  Result := TcxRichEditProperties(inherited Properties);
end;

procedure TcxDBRichEdit.SetDataBinding(Value: TcxDBRichEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBRichEdit.SetProperties(Value: TcxRichEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBRichEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

{ TcxDBRichEditDataBinding }

procedure TcxDBRichEditDataBinding.UpdateData;
begin
  StoredValue := Edit.EditValue;
end;

end.
