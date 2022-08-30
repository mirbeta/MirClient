{***************************************************************************}
{ Property preferences storage                                              }
{ for Delphi & C++Builder                                                   }
{ version 1.0                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2002 - 2006                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}

unit PlanPropPref;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, TypInfo;

procedure StoreProperties(AComponent: TComponent; AStream: TStream; Position: Boolean);
procedure RestoreProperties(AComponent:TComponent; AStream: TStream; Position: Boolean);

procedure StorePropertiesToFile(AComponent: TComponent; FileName: string);
procedure RestorePropertiesToFile(AComponent:TComponent; FileName: string);

implementation

function IsReadAndWriteProp(AInstance: TObject; const APropName:string): Boolean;
begin
  with GetPropInfo(AInstance, APropName)^ do
    Result := (SetProc <> nil) and (GetProc <> nil);
end;

function GetSetPropModified(Instance: TObject; PropInfo: PPropInfo): string;
{Modified GetSetProp routine from TypInfo}
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Result:='';
  Integer(S) := GetOrdProp(Instance, PropInfo);
  TypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
end;

function IsPositionProp(PropName:string): Boolean;
begin
  Result := (PropName = 'Left') or
            (PropName = 'Top');
end;


procedure SaveRTTIComponent(AWriter: TWriter; const Value: TPersistent; Position: Boolean);

  procedure AddProps(AObject: TObject; TypeInfo: PTypeInfo; Level: Integer);
  var
    i,Count: Integer;
    PropList: PPropList;
    SubClass: TClass;
    SubObject: TObject;
    s,propname:string;

  begin
    Count := GetPropList(TypeInfo,tkProperties,nil);
    GetMem(PropList, Count * SizeOf(PPropInfo));
    try
      GetPropList(TypeInfo, tkProperties, PropList);
      for I := 1 to Count do
      begin
        s := string(PropList^[I - 1].PropType^.Name);
        propName := string(PropList^[I - 1]^.Name);


        {$IFDEF TMSDEBUG}
        outputdebugstring(pchar(propname + ': '+s));
        {$ENDIF}

        if IsReadAndWriteProp(AObject,propName) and
          not ((propname = 'Name') and (AObject = Value)) and
          not (not Position and IsPositionProp(PropName) and (AObject = Value)) then
        begin
          case PropList^[I - 1].PropType^.Kind of
          tkFloat:
             AWriter.WriteFloat(GetFloatProp(AObject,PropList^[I - 1]));

          tkInteger,
          tkWChar,
          tkChar:
             AWriter.WriteInteger(GetOrdProp(AObject,PropList^[I - 1]));

          tkString,
          tkLString,
          tkWString:
             AWriter.WriteString(GetStrProp(AObject, PropList^[I - 1]));

          tkEnumeration:
            AWriter.WriteString(GetEnumProp(AObject, PropList^[I - 1]));

          tkSet:
            AWriter.WriteString(GetSetPropModified(AObject,PropList^[I - 1]));
          end;

          if PropList^[I - 1].PropType^.Kind in [tkClass] then
          begin
             SubClass := GetTypeData(PropList^[I - 1].PropType^).ClassType;
             SubObject := TObject(GetOrdProp(AObject,string(PropList^[I - 1].Name)));
             if Assigned(SubObject) then
               AddProps(SubObject,SubClass.ClassInfo,Level + 1);
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;

begin
  AWriter.WriteString(Value.ClassName);
  AddProps(Value,Value.ClassInfo,0);
end;

procedure LoadRTTIComponent(AReader: TReader; const Value: TPersistent; Position: Boolean);

  procedure LoadProps(AObject: TObject; TypeInfo: PTypeInfo; Level: Integer);
  var
    i,Count: Integer;
    PropList: PPropList;
    SubClass: TClass;
    SubObject: TObject;
    s,propname:string;
  begin
    Count := GetPropList(TypeInfo,tkProperties,nil);
    GetMem(PropList, Count * SizeOf(PPropInfo));
    try
      GetPropList(TypeInfo, tkProperties, PropList);
      for I := 1 to Count do
      begin
        s := string(PropList^[I - 1].PropType^.Name);
        propName := string(PropList^[I - 1]^.Name);

        {$IFDEF TMSDEBUG}
        outputdebugstring(pchar('read:'+propname + ': '+s));
        {$ENDIF}

        if IsReadAndWriteProp(AObject,propName) and
          not ((propname = 'Name') and (AObject = Value)) and
          not (not Position and IsPositionProp(PropName) and (AObject = Value)) then
        begin
          case PropList^[I - 1].PropType^.Kind of
          tkFloat: SetFloatProp(AObject, PropList^[I - 1], AReader.ReadFloat);

          tkInteger,
          tkChar,
          tkWChar: SetOrdProp(AObject, PropList^[I - 1], AReader.ReadInteger);

          tkString,
          tkLString,
          tkWString: SetStrProp(AObject, PropList^[I - 1], AReader.ReadString);

          tkEnumeration: SetEnumProp(AObject, PropList^[I - 1], AReader.ReadString);

          tkSet:
            begin
              s := AReader.ReadString;
              if (s <> '') then
                SetSetProp(AObject, PropList^[I - 1], s);
            end;
          end;

          if PropList^[I - 1].PropType^.Kind in [tkClass] then
          begin
             SubClass := GetTypeData(PropList^[I - 1].PropType^).ClassType;
             SubObject := TObject(GetOrdProp(AObject,string(PropList^[I - 1].Name)));
             if Assigned(SubObject) then
               LoadProps(SubObject,SubClass.ClassInfo,Level + 1);
          end;
        end;
      end;
    finally
      FreeMem(PropList);
    end;
  end;

begin
  if AReader.ReadString <> Value.ClassName then
    raise Exception.Create('Class preferences of different class')
  else
    LoadProps(Value,Value.ClassInfo,0);
end;

procedure StoreProperties(AComponent: TComponent; AStream: TStream; Position: Boolean);
var
  writer: TWriter;
begin
  writer := TWriter.Create(AStream, $FF);
  try
    writer.WriteListBegin;
    SaveRTTIComponent(writer,AComponent, Position);
    writer.WriteListEnd;
  finally
    writer.Free;
  end;
end;

procedure RestoreProperties(AComponent: TComponent; AStream: TStream; Position: Boolean);
var
  reader: TReader;
begin
  reader := TReader.Create(AStream, $FF);
  try
    reader.ReadListBegin;
    LoadRTTIComponent(reader,AComponent, Position);
    reader.ReadListEnd;
  finally
    reader.Free;
  end;
end;

procedure StorePropertiesToFile(AComponent: TComponent; FileName: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  StoreProperties(AComponent,ms,false);
  ms.SaveToFile(FileName);
  ms.Free;
end;

procedure RestorePropertiesToFile(AComponent:TComponent; FileName: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(FileName);
  ReStoreProperties(AComponent,ms,false);
  ms.Free;
end;


end.


