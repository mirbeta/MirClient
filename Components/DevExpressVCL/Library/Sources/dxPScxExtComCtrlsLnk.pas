{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPScxExtComCtrlsLnk;

interface

{$I cxVer.inc}

uses
  Windows, Classes, Graphics, Controls, ComCtrls, ImgList, cxListView, cxTreeView,
  cxRichEdit, cxDBRichEdit,
{$IFDEF REGISTERCXSHELLCTRLS}
  cxShellTreeView, cxShellListView,
{$ENDIF}
  dxPSCore, dxPSTVLnk, dxPSLVLnk, dxPSRELnk, dxPSContainerLnk;

type
  TcxCustomTreeViewReportLink = class(TCustomdxTreeViewReportLink)
  private
    function GetcxCustomTreeView: TcxCustomTreeView;
  protected
    function GetCustomTreeView: TCustomTreeView; override;
    property cxCustomTreeView: TcxCustomTreeView read GetcxCustomTreeView;
  end;

  TcxTreeViewReportLink = class(TcxCustomTreeViewReportLink)
  private
    function GetcxTreeView: TcxTreeView;
  public
    property cxTreeView: TcxTreeView read GetcxTreeView;
  published
    property AutoNodesExpand;
    property AutoWidth;
    property Color;
    property ExpandLevel;
    property ExplicitTreeViewExpand;
    property Font;
    property GridLineColor;
    property Options;
    property ScaleFonts;
    property SupportedCustomDraw;
    property Transparent;
    property TreeLineColor;
    property UseVertDelimiters;
    property Width;

    property OnCustomDrawNode;
    property OnInitializeNode;
  end;

 {$IFDEF REGISTERCXSHELLCTRLS}

  TcxShellTreeViewReportLink = class(TcxCustomTreeViewReportLink)
  private
    function GetcxShellTreeView: TcxShellTreeView;
  protected
    function GetCustomTreeView: TCustomTreeView; override;
    procedure CreateImages; override;
    procedure DeleteImages; override;
    function GetImages: TCustomImageList; override;
    function GetStateImages: TCustomImageList; override;
    function HasStateImages: Boolean; override;
  public
    property cxShellTreeView: TcxShellTreeView read GetcxShellTreeView;
  published
    property AutoNodesExpand;
    property AutoWidth;
    property Color;
    property ExpandLevel;
    property ExplicitTreeViewExpand;
    property Font;
    property GridLineColor;
    property Options;
    property ScaleFonts;
    property SupportedCustomDraw;
    property Transparent;
    property TreeLineColor;
    property UseVertDelimiters;
    property Width;

    property OnCustomDrawNode;
    property OnInitializeNode;
  end;

 {$ENDIF}

  TcxCustomListViewReportLink = class(TCustomdxListViewReportLink)
  private
    function GetcxCustomListView: TcxCustomListView;
  protected
    function GetCustomListView: TCustomListView; override;
    property cxCustomListView: TcxCustomListView read GetcxCustomListView;
  end;

  TcxListViewReportLink = class(TcxCustomListViewReportLink)
  private
    function GetcxListView: TcxListView;
  public
    property cxListView: TcxListView read GetcxListView;
  published
    property AutoWidth;
    property Color;
    property DrawMode write SetDrawMode;
    property Effects3D;
    property EndEllipsis;
    property EvenColor;
    property EvenFont;
    property HeaderColor;
    property Font;
    property HeaderFont;
    property HeaderTransparent;
    property GridLineColor;
    property IncludeHeaders;
    property OddColor;
    property OddFont;
    property OnlySelected;
    property Options;
    property RowAutoHeight;
    property Soft3D;
    property SupportedCustomDraw;
    property Transparent;

    property OnCustomDrawHeader;
    property OnCustomDrawItem;
    property OnInitializeHeader;
    property OnInitializeItem;
  end;

 {$IFDEF REGISTERCXSHELLCTRLS}

  TcxShellListViewReportLink = class(TcxCustomListViewReportLink)
  private
    function GetcxShellListView: TcxShellListView;
  protected
    function GetCustomListView: TCustomListView; override;

    procedure CreateImages; override;
    procedure DeleteImages; override;
    function GetLargeImages: TCustomImageList; override;
    function GetSmallImages: TCustomImageList; override;
    function GetStateImages: TCustomImageList; override;
    function HasStateImages: Boolean; override;
  public
    property cxShellListView: TcxShellListView read GetcxShellListView;
  published
    property AutoWidth;
    property Color;
    property DrawMode write SetDrawMode;
    property Effects3D;
    property EndEllipsis;
    property EvenColor;
    property EvenFont;
    property Font;
    property HeaderColor;
    property HeaderFont;
    property HeaderTransparent;
    property GridLineColor;
    property OddColor;
    property OddFont;
    property OnlySelected;
    property Options;
    property RowAutoHeight;
    property IncludeHeaders;
    property ShowColumnHeaders;
    property Soft3D;
    property SupportedCustomDraw;
    property Transparent;

    property OnCustomDrawHeader;
    property OnCustomDrawItem;
    property OnInitializeHeader;
    property OnInitializeItem;
  end;

 {$ENDIF}


  TcxCustomRichEditReportLink = class(TAbstractdxRichEditReportLink)
  private
    function GetRichEdit: TcxCustomRichEdit;
  protected
    function GetRichEditHandle: THandle; override;
    property RichEdit: TcxCustomRichEdit read GetRichEdit;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TcxRichEditReportLink = class(TcxCustomRichEditReportLink)
  private
    function GetRichEdit: TcxRichEdit;
  public
    property RichEdit: TcxRichEdit read GetRichEdit;
  end;


implementation

uses
  cxControls, dxPSUtl;

{ TcxCustomTreeViewReportLink }

function TcxCustomTreeViewReportLink.GetCustomTreeView: TCustomTreeView;
begin
  Result := cxCustomTreeView.InnerTreeView;
end;

function TcxCustomTreeViewReportLink.GetcxCustomTreeView: TcxCustomTreeView;
begin
  Result := Component as TcxCustomTreeView;
end;

{ TcxTreeViewReportLink }

function TcxTreeViewReportLink.GetcxTreeView: TcxTreeView;
begin
  Result := Component as TcxTreeView;
end;

{$IFDEF REGISTERCXSHELLCTRLS}

{ TcxShellTreeViewReportLink }

function TcxShellTreeViewReportLink.GetCustomTreeView: TCustomTreeView;
begin
  Result := cxShellTreeView.InnerTreeView;
end;

procedure TcxShellTreeViewReportLink.CreateImages;
begin
end;

procedure TcxShellTreeViewReportLink.DeleteImages;
begin
end;

function TcxShellTreeViewReportLink.GetImages: TCustomImageList;
begin
  Result := dxPSUtl.ShellSmallImages;
end;

function TcxShellTreeViewReportLink.GetStateImages: TCustomImageList;
begin
  Result := dxPSUtl.ShellSmallImages;
end;

function TcxShellTreeViewReportLink.HasStateImages: Boolean;
begin
  Result := False;
end;

function TcxShellTreeViewReportLink.GetcxShellTreeView: TcxShellTreeView;
begin
  Result := TcxShellTreeView(Component);
end;

{$ENDIF}

{ TcxCustomListViewReportLink }

function TcxCustomListViewReportLink.GetCustomListView: TCustomListView;
begin
  Result := cxCustomListView.InnerListView;
end;

function TcxCustomListViewReportLink.GetcxCustomListView: TcxCustomListView;
begin
  Result := TcxCustomListView(Component);
end;

{ TcxListViewReportLink }

function TcxListViewReportLink.GetcxListView: TcxListView;
begin
  Result := TcxListView(Component);
end;

{$IFDEF REGISTERCXSHELLCTRLS}

{ TcxShellListViewReportLink }

function TcxShellListViewReportLink.GetCustomListView: TCustomListView;
begin
  Result := cxShellListView.InnerListView;
end;

procedure TcxShellListViewReportLink.CreateImages;
begin
end;

procedure TcxShellListViewReportLink.DeleteImages;
begin
end;

function TcxShellListViewReportLink.GetLargeImages: TCustomImageList;
begin
  Result := dxPSUtl.ShellLargeImages;
end;

function TcxShellListViewReportLink.GetSmallImages: TCustomImageList;
begin
  Result := dxPSUtl.ShellSmallImages;
end;

function TcxShellListViewReportLink.GetStateImages: TCustomImageList;
begin
  Result := dxPSUtl.ShellSmallImages;
end;

function TcxShellListViewReportLink.HasStateImages: Boolean;
begin
  Result := not IsReportStyle or inherited HasStateImages;
end;

function TcxShellListViewReportLink.GetcxShellListView: TcxShellListView;
begin
  Result := TcxShellListView(Component);
end;

{$ENDIF}


{ TcxCustomRichEditReportLink }

constructor TcxCustomRichEditReportLink.Create(AOwner: TComponent);
begin
  inherited;
  RichEditVersion := 2;
  LinkModified(False);
end;

function TcxCustomRichEditReportLink.GetRichEditHandle: THandle;
begin
  if RichEdit <> nil then
    Result := RichEdit.InnerControl.Handle
  else
    Result := 0;
end;

function TcxCustomRichEditReportLink.GetRichEdit: TcxCustomRichEdit;
begin
  Result := inherited Component as TcxCustomRichEdit;
end;

{ TcxRichEditReportLink }

function TcxRichEditReportLink.GetRichEdit: TcxRichEdit;
begin
  Result := inherited Component as TcxRichEdit;
end;


initialization
  dxPSRegisterReportLink(TcxTreeViewReportLink, TcxTreeView, TdxfmTVReportLinkDesignWindow);
 {$IFDEF REGISTERCXSHELLCTRLS}
  dxPSRegisterReportLink(TcxShellTreeViewReportLink, TcxShellTreeView, TdxfmTVReportLinkDesignWindow);
 {$ENDIF}

  dxPSRegisterReportLink(TcxListViewReportLink, TcxListView, TdxfmLVReportLinkDesignWindow);
 {$IFDEF REGISTERCXSHELLCTRLS}
  dxPSRegisterReportLink(TcxShellListViewReportLink, TcxShellListView, TdxfmLVReportLinkDesignWindow);
 {$ENDIF}

  dxPSRegisterReportLink(TcxRichEditReportLink, TcxRichEdit, nil);

finalization
  dxPSUnregisterReportLink(TcxRichEditReportLink, TcxRichEdit, nil);

 {$IFDEF REGISTERCXSHELLCTRLS}
  dxPSUnregisterReportLink(TcxShellListViewReportLink, TcxShellListView, TdxfmTVReportLinkDesignWindow);
 {$ENDIF}
  dxPSUnregisterReportLink(TcxListViewReportLink, TcxListView, TdxfmTVReportLinkDesignWindow);

 {$IFDEF REGISTERCXSHELLCTRLS}
  dxPSUnregisterReportLink(TcxShellTreeViewReportLink, TcxShellTreeView, TdxfmLVReportLinkDesignWindow);
 {$ENDIF}
  dxPSUnregisterReportLink(TcxTreeViewReportLink, TcxTreeView, TdxfmLVReportLinkDesignWindow);

end.

