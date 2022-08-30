{************************************************************************}
{ TSYSMON component                                                      }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{            copyright © 2000 - 2011                                     }
{            Email : info@tmssoftware.com                                }
{            Web : http://www.tmssoftware.com                            }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit sysmde;

interface
{$I TMSDEFS.INC}

uses
  SysMon, Classes, Registry, SysUtils, Windows,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;


type
 TMonObjectProperty = class(TStringProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                         end;

 TMonCounterProperty = class(TStringProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                         end;


implementation

const
 subkey:string = 'System\CurrentControlSet\Control\PerfStats\Enum';


function TMonObjectProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList,paSortList];
end;

procedure TMonObjectProperty.GetValues(Proc: TGetStrProc);
var
 keyhandle:hKey;
 objhandle:hkey;
 index:integer;
 subkeyname:array[0..255] of char;
 dwSize:cardinal;
begin
 if RegOpenKeyEx(HKEY_LOCAL_MACHINE,pchar(subkey),0,KEY_READ,KeyHandle)= ERROR_SUCCESS then
  begin
   index:=0;
   dwSize:=sizeof(subkeyname);
   while ( RegEnumKeyEx(Keyhandle,Index,subkeyname,dwSize,nil,nil,nil,nil) = ERROR_SUCCESS ) do
    begin
      if ( RegOpenKeyEx(KeyHandle,subkeyname,0,KEY_READ,objhandle)= ERROR_SUCCESS) then
       begin
        proc(strpas(subkeyname));
        RegCloseKey(ObjHandle);
       end;
     dwSize:=sizeof(subkeyname);
     inc(index);
    end;
   RegCloseKey(KeyHandle);
  end;

end;



{ TMonCounterProperty }

function TMonCounterProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList,paSortList];
end;

procedure TMonCounterProperty.GetValues(Proc: TGetStrProc);
var
 objname:string;
 keyhandle:hKey;
 objhandle:hkey;
 counthandle:hKey;
 index:integer;
 subkeyname:array[0..255] of char;
 dwSize:cardinal;

begin
 objname:=(GetComponent(0) as TMonObject).SysObject;

 if (objname='') then exit;

 if RegOpenKeyEx(HKEY_LOCAL_MACHINE,pchar(subkey),0,KEY_READ,KeyHandle)= ERROR_SUCCESS then
  begin
   if (RegOpenKeyEx(KeyHandle,pchar(objname),0,KEY_READ,objhandle)= ERROR_SUCCESS) then
     begin
      index:=0;
      dwSize:=sizeof(subkeyname);
      while ( RegEnumKeyEx(Objhandle,Index,subkeyname,dwSize,nil,nil,nil,nil) = ERROR_SUCCESS ) do
       begin
        if (RegOpenKeyEx(objhandle,subkeyname,0,KEY_READ,counthandle)=ERROR_SUCCESS) then
         begin
          proc(strpas(subkeyname));
          RegCloseKey(CountHandle);
         end;
        dwSize:=sizeof(subkeyname);
        inc(index);
       end;
      RegCloseKey(ObjHandle);
    end;
   RegCloseKey(KeyHandle);
  end;

end;


end.
 
