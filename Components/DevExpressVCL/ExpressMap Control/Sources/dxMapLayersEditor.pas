{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxMapLayersEditor;

interface

{$I cxVer.inc}

uses
  Types,
  DesignIntf, DesignWindows, ComponentDesigner, DesignConst, DesignEditors, ColnEdit,
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, cxComponentCollectionEditor,
  ImgList, cxGraphics, Menus, ActnList,
  ComCtrls, ToolWin, ExtCtrls, dxMapLayer;

type
  TfrmMapControlLayersEditor = class(TfrmComponentCollectionEditor)
  private
    function GetLayers: TdxMapLayers;
  protected
    procedure AddItem(AItemClassIndex: Integer); override;
    procedure InitFormEditor; override;
  public
    property Layers: TdxMapLayers read GetLayers;
  end;

implementation

uses
  dxMapImageTileLayer, dxMapItemLayer, dxMapItemFileLayer;

const
  ALayerCount = 3;
  ALayerClasses: array [0..ALayerCount - 1] of TdxMapLayerClass = (
    TdxMapImageTileLayer, TdxMapItemLayer, TdxMapItemFileLayer);
  ALayerDisplayNames: array [0..ALayerCount - 1] of string = (
    'Image Tile Layer', 'Item Layer', 'Item File Layer');

{$R *.dfm}

{ TfrmMapControlLayersEditor }

procedure TfrmMapControlLayersEditor.AddItem(AItemClassIndex: Integer);
begin
  Layers.Add(ALayerClasses[AItemClassIndex]);
end;

function TfrmMapControlLayersEditor.GetLayers: TdxMapLayers;
begin
  Result := Collection as TdxMapLayers;
end;

procedure TfrmMapControlLayersEditor.InitFormEditor;
var
  ARoot, AMenuItem: TMenuItem;
  I: Integer;
begin
  inherited;
  ARoot := pmItemClasses.Items;
  for I := 0 to High(ALayerClasses) do
  begin
    AMenuItem := TMenuItem.Create(ARoot);
    AMenuItem.Tag := I;
    AMenuItem.Caption := ALayerDisplayNames[I];
    AMenuItem.OnClick := acAddExecute;
    ARoot.Add(AMenuItem);
  end;
end;

end.
