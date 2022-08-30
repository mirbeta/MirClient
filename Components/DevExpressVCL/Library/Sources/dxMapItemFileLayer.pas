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

unit dxMapItemFileLayer;

interface

{$I cxVer.inc}

uses
  Types, Classes, Graphics, SysUtils,
  cxGraphics, cxGeometry, dxCore, dxCoreClasses, dxGDIPlusClasses, dxCoreGraphics,
  dxMapControlTypes, dxMapLayer, dxCustomMapItemLayer, dxMapItem;

type
  TdxMapItemFileType = (miftKml, miftShape);

  TdxMapItemFileLayer = class(TdxCustomMapItemLayer)
  private
    FActive: Boolean;
    FLoadedActive: Boolean;
    FFileName: TFileName;
    FFileType: TdxMapItemFileType;
    procedure SetActive(const Value: Boolean);
    procedure SetFileName(const Value: TFileName);
    procedure SetFileType(const Value: TdxMapItemFileType);
  protected
    procedure Loaded; override;

    procedure DoActivate; override;
    procedure DoAssign(Source: TPersistent); override;
    procedure DoDeactivate; override;
  public
    procedure LoadFromFile(const AFileName: string = '');
    procedure LoadFromStream(AFileStream: TStream);
    procedure LoadShapeFileFromStream(AShpStream, ADbfStream: TStream);
  published
    property Active: Boolean read FActive write SetActive default False;
    property AllowHotTrack;
    property FileName: TFileName read FFileName write SetFileName;
    property FileType: TdxMapItemFileType read FFileType write SetFileType default miftKml;
    property InitialMapSize;
    property ItemHint;
    property ItemStyle;
    property ItemStyleHot;
    property ItemStyleSelected;
    property ItemTitleOptions;
    property OnGetItemHint;
  end;

implementation

uses
  dxMapControlKmlFileLoader, dxMapControlShapeFileLoader;

{ TdxMapItemFileLayer }

procedure TdxMapItemFileLayer.LoadFromFile(const AFileName: string = '');
var
  AStream: TMemoryStream;
  ADbfStream, AShapeStream: TMemoryStream;
  AFileExt: string;
  AShapeFileName, ADbfFileName: string;
begin
  Active := False;
  if AFileName <> '' then
    FFileName := AFileName;
  AFileExt := ExtractFileExt(FFileName);

  if FileType = miftKml then
  begin
    AStream := TMemoryStream.Create;
    try
      AStream.LoadFromFile(FFileName);
      LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  end
  else
    if FileType = miftShape then
    begin
      AShapeFileName := ChangeFileExt(FFileName, '.shp');
      ADbfFileName := ChangeFileExt(FFileName, '.dbf');
      if FileExists(ADbfFileName) and FileExists(AShapeFileName) then
      begin
        AShapeStream := TMemoryStream.Create;
        ADbfStream := TMemoryStream.Create;
        try
          AShapeStream.LoadFromFile(AShapeFileName);
          ADbfStream.LoadFromFile(ADbfFileName);
          LoadShapeFileFromStream(AShapeStream, ADbfStream);
        finally
          ADbfStream.Free;
          AShapeStream.Free;
        end;
      end;
    end;
end;

procedure TdxMapItemFileLayer.LoadFromStream(AFileStream: TStream);
begin
  if FileType = miftKml then
    try
      with TdxMapControlKmlFileLoader.Create(Self) do
      try
        LoadFromStream(AFileStream);
      finally
        Free;
      end;
      FActive := True;
    except
      MapItems.Clear;
    end;
end;

procedure TdxMapItemFileLayer.LoadShapeFileFromStream(AShpStream, ADbfStream: TStream);
begin
  FileType := miftShape;
  try
    with TdxMapControlShapeFileLoader.Create(Self) do
    try
      LoadFromStream(AShpStream, ADbfStream);
    finally
      Free;
    end;
    FActive := True;
  except
    MapItems.Clear;
  end;
end;

procedure TdxMapItemFileLayer.Loaded;
begin
  inherited Loaded;
  Active := FLoadedActive;
end;

procedure TdxMapItemFileLayer.DoActivate;
begin
  LoadFromFile;
end;

procedure TdxMapItemFileLayer.DoAssign(Source: TPersistent);
begin
  inherited DoAssign(Source);
  if Source is TdxCustomMapItemLayer then
  begin
    FileName := TdxMapItemFileLayer(Source).FileName;
    FileType := TdxMapItemFileLayer(Source).FileType;
    Active := TdxMapItemFileLayer(Source).Active;
  end;
end;

procedure TdxMapItemFileLayer.DoDeactivate;
begin
  MapItems.Clear;
end;

procedure TdxMapItemFileLayer.SetActive(const Value: Boolean);
begin
  if csLoading in ComponentState then
    FLoadedActive := Value
  else
    if FActive <> Value then
    begin
      FActive := Value;
      if not (csDesigning in ComponentState) then
        if FActive then
          DoActivate
        else
          DoDeactivate;
    end;
end;

procedure TdxMapItemFileLayer.SetFileName(const Value: TFileName);
begin
  if FileName <> Value then
  begin
    FFileName := Value;
    if not (csLoading in ComponentState) then
       Active := False;
  end;
end;

procedure TdxMapItemFileLayer.SetFileType(const Value: TdxMapItemFileType);
begin
  if FFileType <> Value then
  begin
    FFileType := Value;
    if not (csLoading in ComponentState) then
      Active := False;
  end;
end;

end.
