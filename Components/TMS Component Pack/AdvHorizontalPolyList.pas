{*************************************************************************}
{ TAdvHorizontalPolyList                                                    }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010                                             }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvHorizontalPolyList;

interface

{$I TMSDEFS.INC}

uses
  Classes, CustomItemsContainer;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvHorizontalPolyList = class(TCustomItemsContainer)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ShowDesignTimeMessage;
    property AutoSizeMode;
    property AutoSizeType;
    property List;
    property Fill;
    property HorizontalSpacing;
    property VerticalSpacing;
    property ListMargins;
    property ReadOnly;
    property Reorder;
    property ShowFocus;
    property BorderMode;
    property BorderTypes;
    property PictureContainer;
    property ImageList;
    property Version;
    property DragLine;
    property DragLineColor;

    property OnStartDraw;
    property OnEndDraw;
    property OnChange;
    property OnItemCompare;
    property OnItemSelect;
    property OnItemDeSelect;
    property OnItemAppearance;

    property Align;
    property Anchors;
    property Ctl3D;
    property BorderStyle;
    property Constraints;
    property PopupMenu;
    property TabOrder;
    property ParentShowHint;
    property ShowHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property DragKind;
    property DragMode;
    property OnResize;
    property OnDblClick;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property OnEndDrag;
    property OnDragDrop;
    property OnDragOver;
    property OnItemReorder;
    property Visible;
    property TabStop default true;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
    property HandleAppearance;
    property ScrollType;
    property ThumbTracking;
    (*
    {$IFDEF DELPHI2006_LVL}
    property ShowDesignHelper;
    {$ENDIF}
    *)
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property Enabled;
    property Color nodefault;
    {$IFDEF DELPHI2006_LVL}
    property Padding;
    property ParentBackground default False;
    {$ENDIF}
    property ParentBiDiMode;
    property ParentCtl3D;
    property ParentFont;
    property OnCanResize;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnUnDock;
    property Transparent;
    property TextRendering;

    property OnVerticalScroll;
    property OnHorizontalScroll;
  end;

implementation

{ TAdvHorizontalPolyList }

constructor TAdvHorizontalPolyList.Create(AOwner: TComponent);
begin
  inherited;
  Columns := 0;
  Rows := 1;
  AutoSizeType := astHeight;
end;

end.

