{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.View.ViewInfo;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Contnrs, Generics.Defaults, Generics.Collections, dxTypeHelpers,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.Utils.Types;

type
  TdxPageViewInfoCollection = class;

  { TdxPageViewInfo }

  TdxPageViewInfo = class(TdxReferencedObject)
  strict private
    FBounds: TRect;
    FClientBounds: TRect;
    FCommentsBounds: TRect;
    FIndex: Integer;
    FPage: TdxPage;
  public
    constructor Create(APage: TdxPage; AOwner: TdxPageViewInfoCollection);

    property Bounds: TRect read FBounds write FBounds;
    property ClientBounds: TRect read FClientBounds write FClientBounds;
    property CommentsBounds: TRect read FCommentsBounds write FCommentsBounds;
    property Index: Integer read FIndex write FIndex;
    property Page: TdxPage read FPage;
  end;

  { TdxFirstPageAnchor }

  TdxFirstPageAnchor = class abstract;

  { TdxRunningHeightFirstPageAnchor }

  TdxRunningHeightFirstPageAnchor = class(TdxFirstPageAnchor)
  strict private
    FTopInvisibleHeight: Int64;
  public
    property TopInvisibleHeight: Int64 read FTopInvisibleHeight write FTopInvisibleHeight;
  end;

  { TdxFirstPageOffsetFirstPageAnchor }

  TdxFirstPageOffsetFirstPageAnchor = class(TdxFirstPageAnchor)
  strict private
    FPageIndex: Integer;
    FVerticalOffset: Integer;
  public
    property PageIndex: Integer read FPageIndex write FPageIndex;
    property VerticalOffset: Integer read FVerticalOffset write FVerticalOffset;
  end;

  { TdxPageViewInfoCollection }

  TdxPageViewInfoCollection = class(TdxReferencedObjectList<TdxPageViewInfo>)
  public
    procedure ExportTo(const AExporter: IdxDocumentLayoutExporter);

    function First: TdxPageViewInfo;
    function Last: TdxPageViewInfo;
  end;

  TdxRichEditViewPageViewInfoCollection = class(TdxPageViewInfoCollection)
  strict private
    FDocumentLayout: TdxDocumentLayout;
    FIsClearing: Boolean;
    FIsDestroying: Boolean;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure UpdateDocumentLayoutVisiblePages;
    procedure ResetDocumentLayoutVisiblePages;

    property IsClearing: Boolean read FIsClearing;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout);
    destructor Destroy; override;
    procedure Clear; override;

    property DocumentLayout: TdxDocumentLayout read FDocumentLayout;
  end;

implementation


{ TdxPageViewInfo }

constructor TdxPageViewInfo.Create(APage: TdxPage; AOwner: TdxPageViewInfoCollection);
begin
  inherited Create;
  Assert(APage <> nil, 'page = nil');
  FPage := APage;
  AOwner.Add(Self);
end;


{ TdxPageViewInfoCollection }

procedure TdxPageViewInfoCollection.ExportTo(const AExporter: IdxDocumentLayoutExporter);
var
  APageViewInfo: TdxPageViewInfo;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    APageViewInfo := Self[I];
    APageViewInfo.Page.ExportTo(AExporter);
  end;
end;

function TdxPageViewInfoCollection.First: TdxPageViewInfo;
begin
  if Count > 0 then
    Result := inherited First
  else
    Result := nil;
end;

function TdxPageViewInfoCollection.Last: TdxPageViewInfo;
begin
  if Count > 0 then
    Result := inherited Last
  else
    Result := nil;
end;

{ TdxRichEditViewPageViewInfoCollection }

constructor TdxRichEditViewPageViewInfoCollection.Create(ADocumentLayout: TdxDocumentLayout);
begin
  inherited Create;
  Assert(ADocumentLayout <> nil, 'documentLayout = nil');
  FDocumentLayout := ADocumentLayout;
end;

destructor TdxRichEditViewPageViewInfoCollection.Destroy;
begin
  FIsDestroying := True;
  inherited Destroy;
end;

procedure TdxRichEditViewPageViewInfoCollection.Clear;
begin
  FIsClearing := True;
  try
    inherited Clear;
  finally
    FIsClearing := False;
    ResetDocumentLayoutVisiblePages;
  end;
end;

procedure TdxRichEditViewPageViewInfoCollection.Notify(
  Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  case Action of
    lnAdded:
      UpdateDocumentLayoutVisiblePages;
    lnDeleted:
      if not IsClearing then
        UpdateDocumentLayoutVisiblePages;
  end;
end;

procedure TdxRichEditViewPageViewInfoCollection.UpdateDocumentLayoutVisiblePages;
begin
  FDocumentLayout.FirstVisiblePageIndex := First.Index;
  FDocumentLayout.LastVisiblePageIndex := Last.Index;
end;

procedure TdxRichEditViewPageViewInfoCollection.ResetDocumentLayoutVisiblePages;
begin
  if FIsDestroying then
    Exit;
  FDocumentLayout.FirstVisiblePageIndex := -1;
  FDocumentLayout.LastVisiblePageIndex := -1;
end;

end.

