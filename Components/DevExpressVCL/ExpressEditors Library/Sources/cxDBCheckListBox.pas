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

unit cxDBCheckListBox;

interface

{$I cxVer.inc}

uses
  Variants, Windows, Classes, Controls, Forms, Graphics, Messages, StdCtrls,
  SysUtils, cxCheckListBox, cxDataUtils, cxDB, cxDBEdit, cxEdit, cxVariants;

type
  { TcxDBCheckListBox }

  TcxDBCheckListBox = class(TcxCustomCheckListBox)
  private
    function GetDataBinding: TcxDBDataBinding;
    procedure SetDataBinding(Value: TcxDBDataBinding);
  protected
    function GetDataBindingClass: TcxCustomDataBindingClass; override;
  published
    property AllowDblClickToggle;
    property AllowGrayed;
    property Anchors;
    property AutoComplete;
    property AutoCompleteDelay;
    property BiDiMode;
    property Columns;
    property Constraints;
    property DataBinding: TcxDBDataBinding read GetDataBinding
      write SetDataBinding;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditValueFormat;
    property Enabled;
    property Glyph;
    property GlyphCount;
    property Images;
    property ImageLayout;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property Items;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollWidth;
    property ShowChecks;
    property ShowHint;
    property Sorted;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnCheckStatesToEditValue;
    property OnClick;
    property OnClickCheck;
    property OnCompare;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEditValueToCheckStates;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TcxDBCheckListBox }

function TcxDBCheckListBox.GetDataBinding: TcxDBDataBinding;
begin
  Result := TcxDBDataBinding(FDataBinding);
end;

procedure TcxDBCheckListBox.SetDataBinding(Value: TcxDBDataBinding);
begin
  FDataBinding.Assign(Value);
end;

function TcxDBCheckListBox.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxDBDataBinding;
end;

end.
