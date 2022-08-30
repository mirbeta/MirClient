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

unit dxDBZoomTrackBar;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Classes, Controls, Dialogs, Forms, Graphics, Messages,
  StdCtrls, SysUtils, {cxControls,} cxDBEdit, cxEdit, dxZoomTrackBar;

type

  { TdxDBZoomTrackBar }

  TdxDBZoomTrackBar = class(TdxCustomZoomTrackBar)
  private
    function GetActiveProperties: TdxZoomTrackBarProperties;
    function GetDataBinding: TcxDBEditDataBinding;
    function GetProperties: TdxZoomTrackBarProperties;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure SetProperties(Value: TdxZoomTrackBarProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TdxZoomTrackBarProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DataBinding: TcxDBEditDataBinding read GetDataBinding
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
    property Properties: TdxZoomTrackBarProperties read GetProperties
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

{ TdxDBZoomTrackBar }

class function TdxDBZoomTrackBar.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxZoomTrackBarProperties;
end;

class function TdxDBZoomTrackBar.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

function TdxDBZoomTrackBar.GetActiveProperties: TdxZoomTrackBarProperties;
begin
  Result := TdxZoomTrackBarProperties(InternalGetActiveProperties);
end;

function TdxDBZoomTrackBar.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

function TdxDBZoomTrackBar.GetProperties: TdxZoomTrackBarProperties;
begin
  Result := TdxZoomTrackBarProperties(inherited Properties);
end;

procedure TdxDBZoomTrackBar.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TdxDBZoomTrackBar.SetProperties(Value: TdxZoomTrackBarProperties);
begin
  Properties.Assign(Value);
end;

procedure TdxDBZoomTrackBar.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

end.

