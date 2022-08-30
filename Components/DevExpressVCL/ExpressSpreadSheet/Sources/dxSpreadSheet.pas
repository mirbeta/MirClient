{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheet;

{$I cxVer.Inc}

interface

uses
  dxSpreadSheetCore, cxControls;

type

  { TdxSpreadSheet }

  TdxSpreadSheet = class(TdxCustomSpreadSheet)
  published
    property Align;
    property Anchors;
    property BorderStyle default cxcbsNone;
    property Enabled;
    property DialogsLookAndFeel;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property LookAndFeel;
    property OptionsBehavior;
    property OptionsLockedStateImage;
    property OptionsProtection;
    property OptionsView;
    property PageControl;
    property ParentDoubleBuffered;
    property ParentFont;
    property PopupMenu;
    property Styles;
    property Visible;
    //
    property OnActiveCellChanging;
    property OnActiveSheetChanged;
    property OnClick;
    property OnCommentHide;
    property OnCommentShow;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDrawTableViewCell;
    property OnCustomDrawTableViewCommonCell;
    property OnCustomDrawTableViewHeaderCell;
    property OnDataChanged;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEditChanged;
    property OnEdited;
    property OnEditing;
    property OnEditValueChanged;
    property OnEndDock;
    property OnEndDrag;
    property OnGetPassword;
    property OnHistoryChanged;
    property OnHyperlinkExecute;
    property OnInitEdit;
    property OnInitEditValue;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLayoutChanged;
    property OnModifiedChanged;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPrepareLockedStateImage;
    property OnProgress;
    property OnResize;
    property OnScroll;
    property OnSelectionChanged;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

end.


