{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumTreeList                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMTREELIST AND ALL        }
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

unit cxTLdxBarBuiltInMenu;

{$I cxVer.inc}

interface

uses
  Classes, Graphics, cxGraphics, cxControls, cxTL, dxBar, dxBarBuiltInMenu, cxClasses;

type
  TcxTreeListdxBarBuiltInMenu = class(TcxTreeListCustomBuiltInMenu)
  private
    function GetBarPopup: TdxBarPopupMenu;
    function GetBarManager: TdxBarManager;
  protected
    function CreateMenuItem(AOwner: TComponent; const ACaption: string; ACommand: TcxTag;
      AEnabled: Boolean = True; AItemType: TcxTreeListBuiltInMenuItemType = tlmitDefault;
      AChecked: Boolean = False; AImageIndex: Integer = -1; AWithSeparator: Boolean = False;
      AInternal: Boolean = True): TComponent; override;
    procedure CreatePopupMenu; override;
    procedure DestroyPopupMenu; override;
    function GetPopupClass: TComponentClass; override;
    function GetPopupMenu: TComponent; override;
    function Popup(X, Y: Integer): Boolean; override;
  public
    property BarPopup: TdxBarPopupMenu read GetBarPopup;
    property BarManager: TdxBarManager read GetBarManager;
  end;

implementation

uses
  dxGDIPlusClasses, Forms;

var
  APreviousBuiltInMenuClass: TcxTreeListCustomBuiltInMenuClass;

{ TcxTreeListdxBarBuiltInMenu }

function TcxTreeListdxBarBuiltInMenu.CreateMenuItem(AOwner: TComponent;
  const ACaption: string; ACommand: TcxTag; AEnabled: Boolean; AItemType: TcxTreeListBuiltInMenuItemType;
  AChecked: Boolean; AImageIndex: Integer; AWithSeparator: Boolean; AInternal: Boolean): TComponent;

  function GetGlyph: TdxSmartGlyph;
  var
    ABitmap: TBitmap;
  begin
    if AInternal then
    begin
      Result := TdxSmartGlyph.Create;
      ABitmap := TBitmap.Create;
      try
        GetBitmapFromImageList(GetImages(AInternal), AImageIndex, ABitmap);
        Result.Assign(ABitmap);
      finally
        ABitmap.Free;
      end;
    end
    else
      Result := nil;
  end;

var
  AGlyph: TdxSmartGlyph;
begin
  AGlyph := GetGlyph;
  try
    Result := dxBarPopupHelper.CreateMenuItem(AOwner, ACaption, ACommand, AEnabled,
      TdxBarBuiltInMenuItemType(AItemType), AChecked, AImageIndex, AWithSeparator, AGlyph, MenuItemClickHandler);
  finally
    AGlyph.Free;
  end;
end;

procedure TcxTreeListdxBarBuiltInMenu.CreatePopupMenu;
begin
  dxBarPopupHelper.LookAndFeel := TreeList.LookAndFeel;
  dxBarPopupHelper.Images := GetImages(False);
  dxBarPopupHelper.CreatePopupMenu;
end;

procedure TcxTreeListdxBarBuiltInMenu.DestroyPopupMenu;
begin
  dxBarPopupHelper.DestroyPopupMenu;
end;

function TcxTreeListdxBarBuiltInMenu.GetPopupClass: TComponentClass;
begin
  Result := TComponentClass(dxBarPopupHelper.PopupMenu.ClassType);
end;

function TcxTreeListdxBarBuiltInMenu.GetPopupMenu: TComponent;
begin
  Result := dxBarPopupHelper.PopupMenu;
end;

function TcxTreeListdxBarBuiltInMenu.Popup(X, Y: Integer): Boolean;
begin
  dxBarPopupHelper.BiDiMode := TreeList.BiDiMode;
  try
    Result := inherited Popup(X, Y);
  finally
    dxBarPopupHelper.BiDiMode := Application.BiDiMode;
  end;
end;

function TcxTreeListdxBarBuiltInMenu.GetBarPopup: TdxBarPopupMenu;
begin
  Result := dxBarPopupHelper.PopupMenu;
end;

function TcxTreeListdxBarBuiltInMenu.GetBarManager: TdxBarManager;
begin
  Result := dxBarPopupHelper.BarManager;
end;

initialization
  APreviousBuiltInMenuClass := cxTreeListBuiltInMenuClass;
  cxTreeListBuiltInMenuClass := TcxTreeListdxBarBuiltInMenu;

finalization
  cxTreeListBuiltInMenuClass := APreviousBuiltInMenuClass;

end.
