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

unit dxPSdxDBTVLnk;

interface

{$I cxVer.inc}

uses
  DB, dxTree, dxDBTree, dxPSTVLnk;

type
  TdxTreeViewExReportLink = class(TCustomdxTreeViewReportLink)
  private
    function GetdxTreeView: TdxTreeView;
  protected
    procedure InternalRestoreFromOriginal; override;
  public
    property dxTreeView: TdxTreeView read GetdxTreeView;
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

  TdxDBTreeViewReportLink = class(TdxTreeViewExReportLink)
  private
    FBookmark: TBookmark;
    function GetDBTreeView: TdxDBTreeView;
  protected
    procedure PrepareContruct; override;
    procedure UnprepareContruct; override;
  public
    property DBTreeView: TdxDBTreeView read GetDBTreeView;
  end;

implementation

uses
  dxPSCore;

{ TdxTreeViewExReportLink }

function TdxTreeViewExReportLink.GetdxTreeView: TdxTreeView;
begin
  Result := TdxTreeView(Component);
end;

procedure TdxTreeViewExReportLink.InternalRestoreFromOriginal;
begin
  inherited;
  SupportedCustomDraw := dxTreeView.IsCustomDraw;
end;

{ TdxDBTreeViewReportLink }

function TdxDBTreeViewReportLink.GetDBTreeView: TdxDBTreeView;
begin
  Result := TdxDBTreeView(Component);
end;

procedure TdxDBTreeViewReportLink.PrepareContruct;
var
  DataSet: TDataSet;
begin
  inherited;

  if DBTreeView.DataSource <> nil then
    DataSet := DBTreeView.DataSource.DataSet
  else
    DataSet := nil;
  if DataSet <> nil then
  begin
    FBookmark := DataSet.GetBookmark;
    DataSet.DisableControls;
  end;
end;

procedure TdxDBTreeViewReportLink.UnprepareContruct;
var
  DataSet: TDataSet;
begin
  if DBTreeView.DataSource <> nil then
    DataSet := DBTreeView.DataSource.DataSet
  else
    DataSet := nil;

  if DataSet <> nil then
  begin
    if FBookmark <> nil then
    begin
      DataSet.GotoBookmark(FBookmark);
      DataSet.FreeBookmark(FBookmark);
      FBookmark := nil;
    end;
    DataSet.EnableControls;
  end;

  inherited;
end;

initialization
  dxPSRegisterReportLink(TdxTreeViewExReportLink, TdxTreeView, TdxfmTVReportLinkDesignWindow);
  dxPSRegisterReportLink(TdxDBTreeViewReportLink, TdxDBTreeView, TdxfmTVReportLinkDesignWindow);

finalization
  dxPSUnRegisterReportLink(TdxTreeViewReportLink, TdxDBTreeView, TdxfmTVReportLinkDesignWindow);
  dxPSUnRegisterReportLink(TdxTreeViewExReportLink, TdxTreeView, TdxfmTVReportLinkDesignWindow);

end.

