{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressTileControl                                       }
{                                                                    }
{           Copyright (c) 2011-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSTILECONTROL AND ALL            }
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

unit dxTileControl;

{$I cxVer.inc}

interface

uses
  Controls, cxControls, dxCustomTileControl;

type
  TdxTileControl = class(TdxCustomTileControl)
  published
    property ActionBars;
    property Align default alClient;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderStyle default cxcbsNone;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Groups;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Images;
    property Items;
    property LookAndFeel;
    property OptionsBehavior;
    property OptionsDetailAnimate;
    property OptionsItemAnimate;
    property OptionsView;
    property ParentBiDiMode;
    property Style;
    property TabOrder;
    property TabStop;
    property Title;
    property Transparent;
    property Visible;

    property OnActionBarsHide;
    property OnActionBarsShow;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetStoredProperties;
    property OnGetStoredPropertyValue;
    property OnGroupDragBegin;
    property OnGroupDragEnd;
    property OnGroupDragOver;
    property OnInitStoredObject;
    property OnItemActivateDetail;
    property OnItemBeforeCheck;
    property OnItemCheck;
    property OnItemDeactivateDetail;
    property OnItemDeactivatingDetail;
    property OnItemDragBegin;
    property OnItemDragEnd;
    property OnItemDragOver;
    property OnItemFocusChange;
    property OnItemFocusChanging;
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
    property OnResize;
    property OnSetStoredPropertyValue;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation


end.
