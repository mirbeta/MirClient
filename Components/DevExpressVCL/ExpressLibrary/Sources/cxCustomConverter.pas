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
unit cxCustomConverter;

{$I cxVer.inc}

interface

uses
  TypInfo, Classes, SysUtils, dxCore;

type
  EcxUnknownProperty = class(EdxException);
  EcxConverterError = class(EdxException);

  TcxCustomConverter = class;
  TcxCustomConverterClass = class of TcxCustomConverter;

  { TcxCustomConverter }
  TcxCustomConverter = class
  private
    FDestination: TObject;
    FPropertyExceptionEnabled: Boolean;
    FSource: TObject;
    function GetObject(AObject: TObject): TObject;
    function GetPropInfo_(AObject: TObject; const AName: string): PPropInfo;
    function TestProperty(AObject: TObject; const AName: string; ATypeKind: TTypeKind): Boolean;
  protected
    function CanConvert: Boolean; virtual;
    procedure DisablePropertyException;
    procedure DoImport; virtual;
    procedure EnablePropertyException;
    function GetBooleanProperty(AObject: TObject; const AName: string;
      const ADefaultValue: Boolean = False): Boolean;
    function GetCharProperty(AObject: TObject; const AName: string;
      const ADefaultValue: Char = #0): Char;
    function GetClassProperty(AObject: TObject; const AName: string;
      const ADefaultValue: TObject = nil): TObject;
    function GetEnumProperty(AObject: TObject; const AName: string;
      const ADefaultValue: string = ''): string;
    function GetFloatProperty(AObject: TObject; const AName: string;
      const ADefaultValue: Extended = 0): Extended;
    function GetIntegerProperty(AObject: TObject; const AName: string;
      const ADefaultValue: Integer = 0): Integer;
    function GetInterfaceProperty(AObject: TObject; const AName: string;
      const ADefaultValue: IUnknown): IUnknown;
    function GetMethodProperty(AObject: TObject; const AName: string;
      const ADefaultValue: TMethod): TMethod;
    procedure GetSetProperty(AObject: TObject; const AName: string; AValues: TStrings);
    function GetStringProperty(AObject: TObject; const AName: string;
      const ADefaultValue: string = ''): string;
    function GetVariantProperty(AObject: TObject; const AName: string;
      const ADefaultValue: Variant): Variant;
    function GetWideCharProperty(AObject: TObject; const AName: string;
      const ADefaultValue: WideChar = #0): WideChar;
    function GetWideStringProperty(AObject: TObject; const AName: string;
      const ADefaultValue: WideString = ''): WideString;
    procedure PostImport; virtual;
    procedure PreImport; virtual;
  public
    constructor Create(ADestination: TObject); virtual;
    class function GetSourceClassName: string; virtual;
    procedure ImportFrom(ASource: TObject);

    property Destination: TObject read FDestination;
    property Source: TObject read FSource write FSource;
  end;

implementation

{ TcxCustomConverter }

constructor TcxCustomConverter.Create(ADestination: TObject);
begin
  FDestination := ADestination;
end;

class function TcxCustomConverter.GetSourceClassName: string;
begin
  Result := 'TObject';
end;

procedure TcxCustomConverter.ImportFrom(ASource: TObject);
begin
  FSource := ASource;
  if CanConvert then
  begin
    PreImport;
    try
      DoImport;
    finally
      PostImport;
    end;
  end;
end;

function TcxCustomConverter.CanConvert: Boolean;
begin
  Result := True;
end;

procedure TcxCustomConverter.DisablePropertyException;
begin
  FPropertyExceptionEnabled := False;
end;

procedure TcxCustomConverter.DoImport;
begin
end;

procedure TcxCustomConverter.EnablePropertyException;
begin
  FPropertyExceptionEnabled := True;
end;

function TcxCustomConverter.GetBooleanProperty(AObject: TObject; const AName: string;
  const ADefaultValue: Boolean): Boolean;
begin
  if TestProperty(AObject, AName, tkEnumeration) then
    Result := Boolean(GetOrdProp(GetObject(AObject), AName))
  else
    Result := ADefaultValue;
end;

function TcxCustomConverter.GetCharProperty(AObject: TObject; const AName: string;
  const ADefaultValue: Char): Char;
begin
  if TestProperty(AObject, AName, tkChar) then
    Result := Char(GetOrdProp(GetObject(AObject), AName))
  else
    Result := ADefaultValue;
end;

function TcxCustomConverter.GetClassProperty(AObject: TObject; const AName: string;
  const ADefaultValue: TObject = nil): TObject;
begin
  if TestProperty(AObject, AName, tkClass) then
    Result := GetObjectProp(GetObject(AObject), AName)
  else
    Result := ADefaultValue;
end;

function TcxCustomConverter.GetEnumProperty(AObject: TObject; const AName: string;
  const ADefaultValue: string): string;
begin
  if TestProperty(AObject, AName, tkEnumeration) then
    Result := GetEnumProp(GetObject(AObject), AName)
  else
    Result := ADefaultValue;
end;

function TcxCustomConverter.GetFloatProperty(AObject: TObject; const AName: string;
  const ADefaultValue: Extended): Extended;
begin
  if TestProperty(AObject, AName, tkFloat) then
    Result := GetFloatProp(GetObject(AObject), AName)
  else
    Result := ADefaultValue;
end;

function TcxCustomConverter.GetIntegerProperty(AObject: TObject; const AName: string;
  const ADefaultValue: Integer): Integer;
begin
  if TestProperty(AObject, AName, tkInteger) then
    Result := GetOrdProp(GetObject(AObject), AName)
  else
    Result := ADefaultValue;
end;

function TcxCustomConverter.GetInterfaceProperty(AObject: TObject; const AName: string;
  const ADefaultValue: IUnknown): IUnknown;
begin
  if TestProperty(AObject, AName, tkInterface) then
    Result := GetInterfaceProp(GetObject(AObject), AName)
  else
    Result := ADefaultValue;
end;

function TcxCustomConverter.GetMethodProperty(AObject: TObject; const AName: string;
  const ADefaultValue: TMethod): TMethod;
begin
  if TestProperty(AObject, AName, tkMethod) then
    Result := GetMethodProp(GetObject(AObject), AName)
  else
    Result := ADefaultValue;
end;

procedure TcxCustomConverter.GetSetProperty(AObject: TObject; const AName: string; AValues: TStrings);
var
  AString: string;
  I: Integer;
begin
  if TestProperty(AObject, AName, tkSet) then
  begin
    if AValues <> nil then
    begin
      AString := GetSetProp(GetObject(AObject), AName);
      if AString <> '' then
      begin
        AValues.Add('');
        for I := 1 to Length(AString) do
        begin
          if AString[I] = ',' then
            AValues.Add('')
          else
            AValues[AValues.Count - 1] := AValues[AValues.Count - 1] + AString[I];
        end;
      end;
    end;
  end;
end;

function TcxCustomConverter.GetStringProperty(AObject: TObject; const AName: string;
  const ADefaultValue: string): string;
begin
  try
    if not TestProperty(AObject, AName, tkString) then
      if not TestProperty(AObject, AName, tkLString) then
      begin
        Result := ADefaultValue;
        Exit;
      end;
  except
    on EcxUnknownProperty do
      TestProperty(AObject, AName, tkLString);
  end;

  Result := GetStrProp(GetObject(AObject), AName);
end;

function TcxCustomConverter.GetVariantProperty(AObject: TObject; const AName: string;
  const ADefaultValue: Variant): Variant;
begin
  if TestProperty(AObject, AName, tkVariant) then
    Result := GetVariantProp(GetObject(AObject), AName)
  else
    Result := ADefaultValue;
end;

function TcxCustomConverter.GetWideCharProperty(AObject: TObject; const AName: string;
  const ADefaultValue: WideChar): WideChar;
begin
  if TestProperty(AObject, AName, tkWChar) then
    Result := WideChar(GetOrdProp(GetObject(AObject), AName))
  else
    Result := ADefaultValue;
end;

function TcxCustomConverter.GetWideStringProperty(AObject: TObject; const AName: string;
  const ADefaultValue: WideString): WideString;
begin
  if TestProperty(AObject, AName, {$IFDEF DELPHI16}tkUString{$ELSE}tkWString{$ENDIF}) then
    Result := GetStrProp(GetObject(AObject), AName)
  else
    Result := ADefaultValue;
end;

procedure TcxCustomConverter.PostImport;
begin
end;

procedure TcxCustomConverter.PreImport;
begin
end;

function TcxCustomConverter.GetObject(AObject: TObject): TObject;
begin
  if AObject <> nil then
    Result := AObject
  else
    Result := FSource;
end;

function TcxCustomConverter.GetPropInfo_(AObject: TObject; const AName: string): PPropInfo;
begin
  Result := GetPropInfo(GetObject(AObject).ClassInfo, AName);
end;

function TcxCustomConverter.TestProperty(AObject: TObject; const AName: string;
  ATypeKind: TTypeKind): Boolean;
var
  APPropInfo: PPropInfo;
begin
  Result := True;
  APPropInfo := GetPropInfo_(AObject, AName);
  if APPropInfo <> nil then
  begin
    if APPropInfo^.PropType^.Kind <> ATypeKind then
    begin
      if FPropertyExceptionEnabled then
        raise EcxUnknownProperty.Create('Invalid property type')
      else
        Result := False;
    end;
  end
  else
  begin
    if FPropertyExceptionEnabled then
      raise EcxUnknownProperty.Create('Invalid property name')
    else
      Result := False;
  end;
end;

end.
