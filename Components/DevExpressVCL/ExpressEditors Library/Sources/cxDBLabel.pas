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

unit cxDBLabel;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Classes, Controls, DB, Dialogs, Forms, Graphics, Messages,
  StdCtrls, SysUtils, cxControls, cxDBEdit, cxEdit, cxLabel;

type
  { TcxDBLabel }

  TcxDBLabel = class(TcxCustomLabel)
  private
    function GetActiveProperties: TcxLabelProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxLabelProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxLabelProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure Initialize; override;
    procedure SetEditAutoSize(Value: Boolean); override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxLabelProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize default False;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding
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
    property Properties: TcxLabelProperties read GetProperties
      write SetProperties;
    property ShowHint;
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
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
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

uses
  VDBConsts;

{ TcxDBLabel }

class function TcxDBLabel.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxLabelProperties;
end;

class function TcxDBLabel.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

procedure TcxDBLabel.Initialize;
begin
  inherited Initialize;
  AutoSize := False;
end;

procedure TcxDBLabel.SetEditAutoSize(Value: Boolean);
begin
  if Value and Assigned(DataBinding) and Assigned(DataBinding.DataLink) and
    DataBinding.DataLink.DataSourceFixed then
      DatabaseError(SDataSourceFixed);
  inherited SetEditAutoSize(Value);
end;

function TcxDBLabel.GetActiveProperties: TcxLabelProperties;
begin
  Result := TcxLabelProperties(InternalGetActiveProperties);
end;

function TcxDBLabel.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

function TcxDBLabel.GetProperties: TcxLabelProperties;
begin
  Result := TcxLabelProperties(inherited Properties);
end;

procedure TcxDBLabel.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBLabel.SetProperties(Value: TcxLabelProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxDBLabel.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

end.
