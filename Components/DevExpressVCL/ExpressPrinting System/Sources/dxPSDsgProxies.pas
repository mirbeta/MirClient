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

unit dxPSDsgProxies;

interface

{$I cxVer.inc}

uses
  DesignIntf, DesignEditors, ComponentDesigner, Classes, ImgList, Controls, ComCtrls, dxBase, dxPSSngltn;

const
  NodeUncheckedStateIndex = 0;
  NodeCheckedStateIndex = 1;
  NodeParticallyCheckedStateIndex = 2;

type
  TFormDesigner = IDesigner;
  IPersistent = TPersistent;
  IComponent = TComponent;

  TdxDesignSelectionList = IDesignerSelections;

  TdxDesignEnvironment = IDesignEnvironment;

function IdeEnvironment: TdxDesignEnvironment;

{ helpers }
function CreateDesignSelectionList: TdxDesignSelectionList;
procedure FreeDesignSelectionList(const ASelections: TdxDesignSelectionList);
procedure RestoreDesignSelection(const ADesigner: TFormDesigner; var AList: TdxDesignSelectionList);
procedure SaveDesignSelection(const ADesigner: TFormDesigner; out AList: TdxDesignSelectionList);
function GetBaseRegistryKey: string;

type
  TdxIdeComponentImageItem = class
  private
    FComponentClass: TComponentClass;
    FImageIndex: Integer;
  public
    constructor Create(AImageIndex: Integer; AComponentClass: TComponentClass);
    property ComponentClass: TComponentClass read FComponentClass;
    property ImageIndex: Integer read FImageIndex;
  end;

  TdxIdeImagesProvider = class(TBasedxPSSingleton)
  private
    FImages: TCustomImageList;
    FItems: TList;
    function GetCount: Integer;
    function GetImageIndex(ComponentClass: TComponentClass): Integer;
    function GetImageIndexByComponentClassName(const ClassName: string): Integer;
    function GetImageIndexByObject(AnObject: TPersistent): Integer;
    function GetItem(Index: Integer): TdxIdeComponentImageItem;
  protected
    procedure FinalizeInstance; override;
    procedure InitializeInstance; override;

    procedure Add(AImageIndex: Integer; AComponentClass: TComponentClass);
    procedure ClearItems;
    function ImageIndexByComponentClass(AComponentClass: TComponentClass): Integer;
    property Items[Index: Integer]: TdxIdeComponentImageItem read GetItem;
  public
    class function Instance: TdxIdeImagesProvider; reintroduce; overload;
    procedure Refresh;

    property Count: Integer read GetCount;
    property ImageIndexes[ComponentClass: TComponentClass]: Integer read GetImageIndex; default;
    property ImageIndexesByComponentClassName[const ClassName: string]: Integer read GetImageIndexByComponentClassName;
    property ImageIndexesByObject[AnObject: TPersistent]: Integer read GetImageIndexByObject;
    property Images: TCustomImageList read FImages;
  end;

function dxIdeImagesProvider: TdxIdeImagesProvider;

implementation

uses
  ToolsAPI, PaletteAPI, Forms, Windows, Graphics, StdCtrls, SysUtils, dxPSCore, dxPSUtl, cxGraphics;

type
  IdxIDEPaletteItem = IInternalPaletteItem;
  IdxIDEPalettePaint = INTAPalettePaintIcon;

function IdeEnvironment: TdxDesignEnvironment;
begin
  if ActiveDesigner <> nil then
    Result := ActiveDesigner.Environment
  else
    Result := nil;
end;

function CreateDesignSelectionList: TdxDesignSelectionList;
begin
  Result := CreateSelectionList;
end;

procedure FreeDesignSelectionList(const ASelections: TdxDesignSelectionList);
begin
end;

procedure RestoreDesignSelection(const ADesigner: TFormDesigner; var AList: TdxDesignSelectionList);
begin
  ADesigner.SetSelections(AList);
  FreeDesignSelectionList(AList);
end;

procedure SaveDesignSelection(const ADesigner: TFormDesigner; out AList: TdxDesignSelectionList);
begin
  AList := CreateDesignSelectionList;
  ADesigner.GetSelections(AList);
end;

function GetBaseRegistryKey: string;
begin
  Result := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey;
end;

{ TdxIdeComponentImageItem }

constructor TdxIdeComponentImageItem.Create(AImageIndex: Integer; AComponentClass: TComponentClass);
begin
  inherited Create;
  FComponentClass := AComponentClass;
  FImageIndex :=  AImageIndex;
end;

{ TdxIdeImagesProvider }

function dxIdeImagesProvider: TdxIdeImagesProvider;
begin
  Result := TdxIdeImagesProvider.Instance;
end;

class function TdxIdeImagesProvider.Instance: TdxIdeImagesProvider;
begin
  Result := inherited Instance as TdxIdeImagesProvider;
end;

procedure TdxIdeImagesProvider.FinalizeInstance;
begin
  FImages.Free;
  ClearItems;
  FItems.Free;
  inherited;
end;

procedure TdxIdeImagesProvider.InitializeInstance;
begin
  inherited;
  FImages := TImageList.CreateSize(CompIconSize + 1, CompIconSize + 1); // plus one for frame
  FItems := TList.Create;
end;

procedure TdxIdeImagesProvider.Add(AImageIndex: Integer; AComponentClass: TComponentClass);
begin
  FItems.Add(TdxIdeComponentImageItem.Create(AImageIndex, AComponentClass));
end;

procedure TdxIdeImagesProvider.ClearItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  FItems.Clear;
end;

function TdxIdeImagesProvider.ImageIndexByComponentClass(AComponentClass: TComponentClass): Integer;
var
  I: Integer;
  Item: TdxIdeComponentImageItem;
begin
  Result := -1;
  if AComponentClass <> nil then
  begin
    if AComponentClass.InheritsFrom(TFrame) then
      AComponentClass := TFrame;
    if AComponentClass.InheritsFrom(TForm) then
      AComponentClass := TForm;

    for I := 0 to Count - 1 do
    begin
      Item := Items[I];
      if AComponentClass = Item.ComponentClass then
      begin
        Result := Item.ImageIndex;
        Exit;
      end;
    end;
  end;
end;

procedure TdxIdeImagesProvider.Refresh;

  { TPaintItemStyle = set of (piDown, piSelected, piFramed, piGosted);}

  function GetComponentImage(APaletteItem: IdxIDEPaletteItem; ABitmap: TcxBitmap): Boolean;
  var
    R: TRect;
    Painter: IdxIDEPalettePaint;
  begin
    R := ABitmap.ClientRect;
    ABitmap.cxCanvas.FillRect(R, clBtnFace);
    Result := Supports(APaletteItem, IdxIDEPalettePaint, Painter);
    if Result then
    begin
      Painter.Paint(ABitmap.Canvas, 0, 0, pi24x24);
      DrawEdge(ABitmap.Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
    end;
  end;

  function AddComponentToImages(ABitmap: TcxBitmap; AComponentClass: TComponentClass): Integer;
  var
    PaletteItem: IdxIDEPaletteItem;
  begin
    Result := -1;
    if IdeEnvironment = nil then Exit;

    PaletteItem := IdeEnvironment.GetPaletteItem(AComponentClass);
    if (PaletteItem <> nil) and GetComponentImage(PaletteItem, ABitmap) then
    begin
      FImages.Add(ABitmap, nil);
      Result := FImages.Count - 1;
    end;
  end;

var
  List: TdxClassList;
  Bitmap: TcxBitmap;
  I, ImageIndex: Integer;
  ComponentClass: TComponentClass;
begin
  FImages.Clear;
  ClearItems;

  List := TdxClassList.Create;
  try
    Bitmap := TcxBitmap.CreateSize(FImages.Width, FImages.Height, pf24bit);
    try
      dxPSCore.dxPSGetSupportedComponentsList(List);
      for I := 0 to List.Count - 1 do
      begin
        ComponentClass := TComponentClass(List[I]);
        if ComponentClass <> nil then
        begin
          ImageIndex := AddComponentToImages(Bitmap, ComponentClass);
          Add(ImageIndex, ComponentClass);
        end;
      end;
    finally
      Bitmap.Free;
    end;
  finally
    List.Free;
  end;
end;

function TdxIdeImagesProvider.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxIdeImagesProvider.GetImageIndex(ComponentClass: TComponentClass): Integer;
begin
  Result := ImageIndexByComponentClass(ComponentClass);
end;

function TdxIdeImagesProvider.GetImageIndexByComponentClassName(const ClassName: string): Integer;
var
  AClass: TClass;
begin
  AClass := Classes.GetClass(ClassName);
  if AClass.InheritsFrom(TComponent) then
    Result := ImageIndexes[TComponentClass(AClass)]
  else
    Result := -1;
end;

function TdxIdeImagesProvider.GetImageIndexByObject(AnObject: TPersistent): Integer;
begin
  if AnObject is TComponent then
    Result := ImageIndexes[TComponentClass(AnObject.ClassType)]
  else
    Result := -1;
end;

function TdxIdeImagesProvider.GetItem(Index: Integer): TdxIdeComponentImageItem;
begin
  Result := TdxIdeComponentImageItem(FItems[Index]);
end;

initialization

finalization
  TdxIdeImagesProvider.ReleaseInstance;

end.

