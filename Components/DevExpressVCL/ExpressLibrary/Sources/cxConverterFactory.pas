{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
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
unit cxConverterFactory;

{$I cxVer.inc}

interface

uses
  DesignIntf, Forms, SysUtils, cxStyles, Classes,
  dxCore, cxClasses, cxLibraryStrs, cxDesignWindows, cxCustomConverter;

type
  TcxCustomConverterWithStyles = class;
  TcxCustomConverterWithStylesClass = class of TcxCustomConverterWithStyles;

  { TcxCustomConverterWithStyles }
  TcxCustomConverterWithStyles = class(TcxCustomConverter)
  private
    FConvertWithStyles: Boolean;
    FDesigner: IDesigner;
    FNameOfNewStyleRepository: string;
    FStyleRepository: TcxStyleRepository;
    function GetStyleRepository: TcxStyleRepository;
  protected
    function CreateStyleItem: TcxCustomStyle;
    procedure DoImport; override;
    procedure DoImportStyles; virtual;
  public
    property ConvertWithStyles: Boolean read FConvertWithStyles write FConvertWithStyles;
    property Designer_: IDesigner read FDesigner write FDesigner;
    property NameOfNewStyleRepository: string read FNameOfNewStyleRepository write FNameOfNewStyleRepository;
    property StyleRepository: TcxStyleRepository read GetStyleRepository write FStyleRepository;
  end;

  { TcxConverterFactoryItem }
  PcxConverterFactoryItem = ^TcxConverterFactoryItem;
  TcxConverterFactoryItem = record
    Class_: TcxCustomConverterWithStylesClass;
    Name: string;
  end;

  { TcxConverterFactory }
  TcxConverterFactory = class
  private
    FItems: TList;
    constructor CreateInstance(AParam: Integer);
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TcxConverterFactoryItem;
    function IndexOf(AClass: TcxCustomConverterWithStylesClass): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function FindConverter(ASource: TObject): TcxCustomConverterWithStylesClass;
    class function Instance(AConverterGroupName: string): TcxConverterFactory;
    procedure RegisterConverter(const AName: string; const AClass: TcxCustomConverterWithStylesClass);
    procedure UnregisterConverter(const AClass: TcxCustomConverterWithStylesClass);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TcxConverterFactoryItem read GetItems; default;
  end;

  function ConverterFactory(AConverterGroupName: string): TcxConverterFactory;

implementation

function ConverterFactory(AConverterGroupName: string): TcxConverterFactory;
begin
  Result := TcxConverterFactory.Instance(AConverterGroupName);
end;

{ TcxCustomConverterWithStyles }

function TcxCustomConverterWithStyles.CreateStyleItem: TcxCustomStyle;
var
  AStyleRepository: TcxStyleRepository;
begin
  AStyleRepository := StyleRepository;
  if AStyleRepository = nil then
    raise EcxConverterError.Create(cxGetResourceString(@scxConverterCantCreateStyleRepository));
  Result := AStyleRepository.CreateItemEx(TcxStyle, AStyleRepository.Owner);
  Result.Name := CreateUniqueName(AStyleRepository.Owner, nil, Result, '', '');
  Designer_.Modified;
end;

procedure TcxCustomConverterWithStyles.DoImport;
begin
  if ConvertWithStyles then
  begin
    EnablePropertyException;
    try
      DoImportStyles;
    finally
      DisablePropertyException;
    end;
  end;
  inherited DoImport;
end;

procedure TcxCustomConverterWithStyles.DoImportStyles;
begin
end;

function TcxCustomConverterWithStyles.GetStyleRepository: TcxStyleRepository;
begin
  if FStyleRepository = nil then
  begin
    FStyleRepository := TcxStyleRepository.Create(Designer_.GetRoot);
    FStyleRepository.Name := FNameOfNewStyleRepository;
    Designer_.Modified;
  end;
  Result := FStyleRepository;
end;

{ TcxConverterFactory }

var
  ConverterFactories: TStringList;

procedure CreateConverterFactories;
begin
  ConverterFactories := TStringList.Create;
end;

procedure FreeConverterFactories;
var
  I: Integer;
begin
  for I := 0 to ConverterFactories.Count - 1 do
    ConverterFactories.Objects[I].Free;
  ConverterFactories.Free;
end;

constructor TcxConverterFactory.Create;
begin
  raise EdxException.CreateFmt('Access class %s through Instance only', [ClassName]);
end;

destructor TcxConverterFactory.Destroy;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    Dispose(PcxConverterFactoryItem(FItems[I]));
  FItems.Free;

  inherited Destroy;
end;

function TcxConverterFactory.FindConverter(ASource: TObject): TcxCustomConverterWithStylesClass;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].Class_;
    if ClassInheritsFrom(ASource.ClassType, Result.GetSourceClassName) then Exit;
  end;
  Result := nil;
end;

class function TcxConverterFactory.Instance(AConverterGroupName: string): TcxConverterFactory;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ConverterFactories.Count - 1 do
  begin
    if ConverterFactories[I] = AConverterGroupName then
    begin
      Result := ConverterFactories.Objects[I] as TcxConverterFactory;
      Break;
    end;
  end;
  if Result = nil then
  begin
    Result := CreateInstance(0);
    ConverterFactories.AddObject(AConverterGroupName, Result);
  end;
end;

procedure TcxConverterFactory.RegisterConverter(const AName: string; const AClass: TcxCustomConverterWithStylesClass);
var
  AP: PcxConverterFactoryItem;
begin
  if IndexOf(AClass) = -1 then
  begin
    New(AP);
    AP^.Name := AName;
    AP^.Class_ := AClass;
    FItems.Add(AP);
  end;
end;

procedure TcxConverterFactory.UnregisterConverter(const AClass: TcxCustomConverterWithStylesClass);
var
  AIndex: Integer;
begin
  AIndex := IndexOf(AClass);
  if AIndex <> -1 then
  begin
    Dispose(PcxConverterFactoryItem(FItems[AIndex]));
    FItems.Delete(AIndex);
  end;
end;

constructor TcxConverterFactory.CreateInstance;
begin
  FItems := TList.Create;
end;

function TcxConverterFactory.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxConverterFactory.GetItems(AIndex: Integer): TcxConverterFactoryItem;
begin
  Result := PcxConverterFactoryItem(FItems[AIndex])^;
end;

function TcxConverterFactory.IndexOf(AClass: TcxCustomConverterWithStylesClass): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].Class_ = AClass then
    begin
      Result := I;
      Break;
    end;
end;

initialization
  CreateConverterFactories;

finalization
  FreeConverterFactories;

end.

