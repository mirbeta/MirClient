{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars converter component                          }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxBarConverter;

{$I cxVer.inc}

interface

uses
  Classes, Menus, dxBar;

procedure dxBarConvertMainMenu(AMenu: TMenu; ABarManager: TdxBarManager);
procedure dxBarConvertPopupMenu(APopupMenu: TPopupMenu; ABarPopupMenu: TdxBarCustomPopupMenu);

implementation

uses
  SysUtils, Controls, Forms, dxBarCustomCustomizationForm;

type
  TdxBarCustomPopupMenuAccess = class(TdxBarCustomPopupMenu);

function AddCategory(ABarManager: TdxBarManager; AName: string): Integer;
begin
  Result := ABarManager.Categories.IndexOf(AName);
  if Result < 0 then
    Result := ABarManager.Categories.Add(AName);
end;

procedure ConvertItems(ABarManager: TdxBarManager; AItemLinks: TdxBarItemLinks;
  AItems: TMenuItem; ACategory: Integer);

  function AddItem(AItem: TMenuItem; AItemLinks: TdxBarItemLinks; ACategory: Integer): TdxBarItemLink;
  var
    ABarItem: TdxBarItem;
    AIsSubItem: Boolean;
    AItemName: string;
  begin
    AIsSubItem := AItem.Count > 0;
    if AIsSubItem then
      Result := AItemLinks.Add(TdxBarSubItem.Create(ABarManager.Owner))
    else
      Result := AItemLinks.Add(TdxBarButton.Create(ABarManager.Owner));

    ABarItem := Result.Item;

    ABarItem.Category := ACategory;
    ABarItem.Action := AItem.Action;
    ABarItem.ImageIndex := AItem.ImageIndex;
    ABarItem.Glyph.Assign(AItem.Bitmap);
    ABarItem.Caption := AItem.Caption;
    ABarItem.Enabled := AItem.Enabled;
    ABarItem.HelpContext := AItem.HelpContext;
    ABarItem.Hint := AItem.Hint;
    ABarItem.ShortCut := AItem.ShortCut;
    ABarItem.Tag := AItem.Tag;
    ABarItem.OnClick := AItem.OnClick;

    if AIsSubItem then
    begin
      TdxBarSubItem(ABarItem).Images := AItem.SubMenuImages;
      ConvertItems(ABarManager, TdxBarSubItem(ABarItem).ItemLinks, AItem, AddCategory(ABarManager, GetTextOf(AItem.Caption)))
    end
    else
    begin
      if AItem.Checked or AItem.RadioItem then
        TdxBarButton(ABarItem).ButtonStyle := bsChecked;
      if AItem.RadioItem then
        TdxBarButton(ABarItem).GroupIndex := AItem.GroupIndex;
      TdxBarButton(ABarItem).Down := AItem.Checked;
    end;

    if csDesigning in ABarManager.ComponentState then
    begin
      AItem.OnClick := nil;
      AItemName := AItem.Name;
      AItem.Name := AItemName + '_old';
      ABarItem.Name := AItemName;
    end;
  end;

var
  ABeginGroup: Boolean;
  I: Integer;
begin
  ABeginGroup := False;
  for I := 0 to AItems.Count - 1 do
  begin
    if AItems[I].Caption = '-' then
      ABeginGroup := True
    else
    begin
      AddItem(AItems[I], AItemLinks, ACategory).BeginGroup := ABeginGroup;
      ABeginGroup := False;
    end;
  end;
end;

procedure dxBarConvertMainMenu(AMenu: TMenu; ABarManager: TdxBarManager);
var
  AItemLinks: TdxBarItemLinks;
begin

  if ABarManager.MainMenuBar = nil then
    with ABarManager.Bars.Add do
    begin
      Caption := 'Main Menu';
      DockingStyle := dsTop;
      IsMainMenu := True;
      Visible := True;
    end;
  AItemLinks := ABarManager.MainMenuBar.ItemLinks;

  if ABarManager.Images = nil then
    ABarManager.Images := AMenu.Images;

  ABarManager.BeginUpdate;
  try
    ConvertItems(ABarManager, AItemLinks, AMenu.Items, AddCategory(ABarManager, AMenu.Name));
  finally
    ABarManager.EndUpdate;
  end;
end;

procedure dxBarConvertPopupMenu(APopupMenu: TPopupMenu; ABarPopupMenu: TdxBarCustomPopupMenu);
var
  AItemLinks: TdxBarItemLinks;
begin
  ABarPopupMenu.OnPopup := APopupMenu.OnPopup;
  AItemLinks := ABarPopupMenu.ItemLinks;
  if TdxBarCustomPopupMenuAccess(ABarPopupMenu).Images = nil then
    TdxBarCustomPopupMenuAccess(ABarPopupMenu).Images := APopupMenu.Images;

  ABarPopupMenu.BarManager.BeginUpdate;
  try
    ConvertItems(ABarPopupMenu.BarManager, AItemLinks, APopupMenu.Items, AddCategory(ABarPopupMenu.BarManager, APopupMenu.Name));
  finally
    ABarPopupMenu.BarManager.EndUpdate;
  end;
end;

end.
