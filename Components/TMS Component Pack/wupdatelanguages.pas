{*******************************************************************}
{ TWEBUPDATE Wizard component                                       }
{ for Delphi & C++Builder                                           }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright © 1998 - 2015                                        }
{    Email : info@tmssoftware.com                                   }
{    Web   : http://www.tmssoftware.com                             }
{                                                                   }
{ The source code is given as is. The author is not responsible     }
{ for any possible damage done due to the use of this code.         }
{ The component can be freely used in any application. The source   }
{ code remains property of the writer and may not be distributed    }
{ freely as such.                                                   }
{*******************************************************************}

unit WUpdateLanguages;

interface

uses
  Classes, WUpdateWiz;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardEnglish = class(TWebUpdateWizardLanguage)
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardDutch = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardFrench = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardGerman = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardPortugese = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardSpanish = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardDanish = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardItalian = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardNorwegian = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardHungarian = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardSwedish = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardCzech = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardPolish = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdateWizardCatalan = class(TWebUpdateWizardLanguage)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TWebUpdateWizardDutch }

constructor TWebUpdateWizardDutch.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Druk start om te beginnen met controleren voor applicatie updates ...';
  StartButton := 'Start';
  NextButton := 'Volgende';
  ExitButton := 'Verlaten';
  CancelButton := 'Annuleren';
  RestartButton := 'Herstarten';
  GetUpdateButton := 'Update';
  NewVersionFound := 'Nieuwe version gevonden';
  NewVersion := 'Nieuwe versie';
  NoNewVersionAvail := 'Geen nieuwe versie beschikbaar.';
  NewVersionAvail := 'Nieuwe versie beschikbaar.';
  CurrentVersion := 'Huidige versie';
  NoFilesFound := 'Geen bestanden gevonden voor update';
  NoUpdateOnServer := 'geen update gevonden op server ...';
  CannotConnect := 'Er kan geen verbinding met de update server tot stand gebracht worden of';
  WhatsNew := 'Nieuw in update';
  License := 'Licentie overeenkomst';
  AcceptLicense := 'Ik aanvaard';
  NotAcceptLicense := 'Ik aanvaard niet';
  ComponentsAvail := 'Beschikbare applicatie componenten';
  DownloadingFiles := 'Downloaden bestanden';
  CurrentProgress := 'Vooruitgang huidig bestand';
  TotalProgress := 'Totale vooruitgang';
  UpdateComplete := 'Update volledig ...';
  RestartInfo := 'Druk Herstarten om de nieuwe versie te starten.';
  WhatsNewPopup := 'Bekijken in kladblok';
  LicensePopup := 'Bekijken in kladblok';
end;

{ TWebUpdateWizardFrench }

constructor TWebUpdateWizardFrench.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Cliquez sur Vérifier pour rechercher d''éventuelles mises à jour ...';
  StartButton := 'Vérifier';
  NextButton := 'Suivant';
  ExitButton := 'Quitter';
  CancelButton := 'Annuler';
  RestartButton := 'Redémarrer';
  GetUpdateButton := 'Mettre à jour';
  NewVersionFound := 'Une nouvelle version est disponible';
  NewVersion := 'Nouvelle version';
  NoNewVersionAvail := 'Pas de nouvelle version disponible.';
  NewVersionAvail := 'Nouvelle version disponible.';
  CurrentVersion := 'Version actuelle';
  NoFilesFound := 'pas de mises à jour disponible';
  NoUpdateOnServer := 'Aucune mise à jour disponible ...';
  CannotConnect := 'Connexion impossible avec le serveur ou';
  WhatsNew := 'Nouveautés';
  License := 'Informations de license';
  AcceptLicense := 'J''accepte';
  NotAcceptLicense := 'Je refuse';
  ComponentsAvail := 'Composants d''application disponible';
  DownloadingFiles := 'Téléchargement des fichiers';
  CurrentProgress := 'Progression du fichier';
  TotalProgress := 'Progression totale';
  UpdateComplete := 'Mise à jour terminée';
  RestartInfo := 'Cliquez sur Redémarrer pour lancer la nouvelle version';
  FailedDownload := 'Echec de la mise à jour';
  LicensePopup := 'Voir avec Notepad';
  WhatsNewPopup := 'Voir avec Notepad';
end;

{ TWebUpdateWizardGerman }

constructor TWebUpdateWizardGerman.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Klicken Sie auf Starten, um mit der Updateprüfung zu beginnen...';
  StartButton := 'Start';
  NextButton := 'Weiter';
  ExitButton := 'Schließen';
  RestartButton := 'Neu starten';
  CancelButton := 'Abbrechen';
  GetUpdateButton := 'Weiter';
  NewVersionFound := 'Es wurde eine neue Version gefunden';
  NewVersion := 'Neue Version';
  NoNewVersionAvail := 'Keine neue Version verfügbar.';
  NewVersionAvail := 'Es ist eine neue Version verfügbar';
  CurrentVersion := 'Ihre Version';
  NoFilesFound := 'Auf dem Server wurden keine Dateien gefunden.';
  NoUpdateOnServer := 'Kein Update vorhanden auf Server ...';
  CannotConnect := 'Die Verbindung zum Server konnte nicht hergestellt werden';
  WhatsNew := 'Was ist neu?';
  License := 'Lizenzvereinbarung';
  AcceptLicense := 'Ich akzeptiere die Vereinbarung' ;
  NotAcceptLicense := 'Ich lehne die Vereinbarung ab';
  ComponentsAvail := 'Verfügbare Anwendungskomponenten';
  DownloadingFiles := 'Lade Dateien';
  CurrentProgress := 'Dateidownload:';
  TotalProgress := 'Gesamter Updateverlauf:';
  UpdateComplete := 'Der Download des Updates ist komplett ...';
  RestartInfo := 'Bestätigen Sie den Neustart,'+chr(13)+chr(10)+'um das Update zu starten.';
  FailedDownload := 'Fehler während download update dateien';
  LicensePopup := 'Zeigen in Notepad';
  WhatsNewPopup := 'Zeigen in Notepad';
end;

{ TWebUpdateWizardPortugese }

constructor TWebUpdateWizardPortugese.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Clique iniciar para verificar se há novas atualizações...';
  StartButton := 'Iniciar';
  NextButton := 'Próximo';
  ExitButton := 'Sair';
  CancelButton := 'Cancelar';
  RestartButton := 'Reiniciar';
  GetUpdateButton := 'Obter atualização';
  NewVersionFound := 'Nova versão encontrada';
  NewVersion := 'Nova versão';
  NoNewVersionAvail := 'Não há novas versões disponíveis.';
  NewVersionAvail := 'Nova versão disponível.';
  CurrentVersion := 'Versão atual';
  NoFilesFound := 'Nenhum arquivo encontrado para a atualização';
  NoUpdateOnServer := 'Nenhuma atualização encontrada no servidor...';
  CannotConnect := 'Não foi possível conectar ao servidor de atualização ou';
  WhatsNew := 'O que há de novo';
  License := 'Contrato de licença';
  AcceptLicense := 'Aceito';
  NotAcceptLicense := 'Não aceito';
  ComponentsAvail := 'Componentes da aplicação disponíveis';
  DownloadingFiles := 'Fazendo o download dos arquivos';
  CurrentProgress := 'Progresso do arquivo atual';
  TotalProgress := 'Progresso total';
  UpdateComplete := 'Atualização concluída...';
  RestartInfo := 'Clique reiniciar para iniciar a aplicação atualizada.';
  FailedDownload := 'Falha no download da atualização';
  LicensePopup := 'Exibir no Notepad';
  WhatsNewPopup := 'Exibir no Notepad';
end;

{ TWebUpdateWizardSpanish }

constructor TWebUpdateWizardSpanish.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Presione iniciar para buscar actualizaciones disponibles de la aplicación ...';
  StartButton := 'Iniciar';
  NextButton := 'Siguiente';
  ExitButton := 'Salir';
  CancelButton := 'Cancelar';
  RestartButton := 'Reiniciar';
  GetUpdateButton := 'Obtener actualización';
  NewVersionFound := 'Nueva versión encontrada';
  NewVersion := 'Nueva versión';
  NoNewVersionAvail := 'No hay una nueva versión disponible.';
  NewVersionAvail := 'Nueva versión disponible.';
  CurrentVersion := 'Versión actual';
  NoFilesFound := 'No se encontraron archivos para actualizar';
  NoUpdateOnServer := 'no se encontró una nueva actualización en el servidor ...';
  CannotConnect := 'No se puedo establecer la conexión con el servidor de actualizaciones o ';
  WhatsNew := 'Lo nuevo';
  License := 'Acuerdo de licenciamiento';
  AcceptLicense := 'Acepto';
  NotAcceptLicense := 'No acepto';
  ComponentsAvail := 'Componentes de la aplicación disponibles';
  DownloadingFiles := 'Descargando archivos';
  CurrentProgress := 'Progreso de archivo actual';
  TotalProgress := 'Progreso total';
  UpdateComplete := 'Actualización completada ...';
  RestartInfo := 'Presione reiniciar para ejecutar la aplicación actualizada.';
end;

{ TWebUpdateWizardDanish }

constructor TWebUpdateWizardDanish.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Tryk på Start-knappen for at checke for applikationsopdateringer ...';
  StartButton := 'Start';
  NextButton := 'Næste';
  ExitButton := 'Afslut';
  CancelButton := 'Fortryd';
  RestartButton := 'Genstart';
  GetUpdateButton := 'Hent opdatering';
  NewVersionFound := 'Ny version blev fundet';
  NewVersion := 'Ny version';
  NoNewVersionAvail := 'Ingen ny version tilgængelig.';
  NewVersionAvail := 'Ny version tilgængelig.';
  CurrentVersion := 'Nuværende version';
  NoFilesFound := 'Ingen opdaterbare filer blev fundet';
  NoUpdateOnServer := 'ingen opdatering blev fundet på serveren ...';
  CannotConnect := 'Kunne ikke få kontakt til opdateringsserveren eller';
  WhatsNew := 'Hvad er nyt?';
  License := 'Licensaftale';
  AcceptLicense := 'Jeg accepterer';
  NotAcceptLicense := 'Jeg accepterer ikke';
  ComponentsAvail := 'Tilgængelige applikationskomponenter';
  DownloadingFiles := 'Downloader filer';
  CurrentProgress := 'Nuværende filforløb';
  TotalProgress := 'Total filforløb';
  UpdateComplete := 'Opdatering fuldført ...';
  RestartInfo := 'Tryk på genstart for at starte den opdaterede applikation.';
end;

{ TWebUpdateWizardItalian }

constructor TWebUpdateWizardItalian.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Premi Inizia per verificare la disponibilità di aggiornamenti dell''applicazione...';
  StartButton := 'Inizia';
  NextButton := 'Avanti';
  ExitButton := 'Esci';
  CancelButton := 'Annulla';
  RestartButton := 'Riavvia';
  GetUpdateButton := 'Scarica l''aggiornamento';
  NewVersionFound := 'Trovata una nuova versione';
  NewVersion := 'Nuova versione';
  NoNewVersionAvail := 'Nessuna nuova versione disponibile.';
  NewVersionAvail := 'Nuova versione disponibile.';
  CurrentVersion := 'Versione corrente';
  NoFilesFound := 'file non trovati per l''aggiornamento';
  NoUpdateOnServer := 'non c''è un nuovo aggiornamento sul server...';
  CannotConnect := 'Impossibile stabilire la connessione con il server o ';
  WhatsNew := 'Novità';
  License := 'Accordo di licenza';
  AcceptLicense := 'Accetto';
  NotAcceptLicense := 'Non accetto';
  ComponentsAvail := 'Componenti dell''applicazione disponibil';
  DownloadingFiles := 'Scarico i file';
  CurrentProgress := 'Avanzamento del file corrente';
  TotalProgress := 'Avanzamento complessivo';
  UpdateComplete := 'Aggiornamento completo...';
  RestartInfo := 'Premi riavvia per eseguire l''applicazione aggiornata.';
end;

{ TWebUpdateWizardNorwegian }

constructor TWebUpdateWizardNorwegian.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Klikk Start for å se etter tilgjengelige oppdateringer av programmet...';
  StartButton := 'Start';
  NextButton := 'Neste';
  ExitButton := 'Avslutt';
  CancelButton := 'Avbryt';
  RestartButton := 'Start på nytt';
  GetUpdateButton := 'Hent oppdatering';
  NewVersionFound:= 'Ny versjon';
  NoNewVersionAvail := 'Ingen ny versjon er tilgjengelig.';
  NewVersionAvail := 'Ny versjon er tilgjengelig for nedlasting.';
  CurrentVersion := 'Nåværende versjon';
  NoFilesFound := 'Fant ingen filer for oppdateringen';
  NoUpdateOnServer := 'fant ingen oppdatering på serveren ...';
  CannotConnect := 'Kunne ikke koble til oppdateringsserveren eller ';
  WhatsNew := 'Hva er nytt';
  License := 'Lisensavtale';
  AcceptLicense := 'Jeg godtar';   //Too long for the current radio button width
  NotAcceptLicense := 'Jeg godtar ikke';  //Too long for the current radio button width
  ComponentsAvail := 'Tilgjengelige programkomponenter';
  DownloadingFiles := 'Laster ned filer';
  CurrentProgress := 'Nedlastingsforløpet for nåværende fil';
  TotalProgress := 'Nedlastingsforløpet for alle filer';
  UpdateComplete := 'Oppdateringen er ferdig ...';
  RestartInfo := 'Klikk Start på nytt  for å starte det oppdaterte programmet.';
end;

{ TWebUpdateWizardHungarian }

constructor TWebUpdateWizardHungarian.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Kattints az indít gombra és elindul a frissítések keresése ...';
  StartButton := 'Indít';
  NextButton := 'Tovább';
  ExitButton := 'Kilépés';
  CancelButton := 'Mégsem';
  RestartButton := 'Újraindít';
  GetUpdateButton := 'Frissít';
  NewVersionFound := 'Új verziót találtam';
  NewVersion := 'Új verzió';
  NoNewVersionAvail := 'Új verzió nem található.';
  NewVersionAvail := 'Új verzió található.';
  CurrentVersion := 'Aktuális verzió';
  NoFilesFound := 'A frissítésben nem található file';
  NoUpdateOnServer := 'nem található frissítés a kiszolgálón ...';
  CannotConnect := 'Nem tudok kapcsolódni a frissítõ kiszolgálóhoz';
  WhatsNew := 'Mi az újdonság';
  License := 'Szerzõdési feltétel';
  AcceptLicense := 'Elfogadom';
  NotAcceptLicense := 'Visszautasítom';
  ComponentsAvail := 'Lehetséges alkalmazás kopmponensek';
  DownloadingFiles := 'Álományok letöltése';
  CurrentProgress := 'Aktuális mûvelet állpota';
  TotalProgress := 'Teljes mûvelet állapota';
  UpdateComplete := 'Frissítés kész ...';
  RestartInfo := 'Kattints az Ujraindít gombra, hogy elinduljon a frissített alkalmazás.';
end;


{ TWebUpdateWizardSwedish }

constructor TWebUpdateWizardSwedish.Create(AOwner: TComponent);
begin
  inherited;
    Welcome := 'Tryck p'#229' Start-knappen f'#246'r att leta efter tillg'#228'ngliga uppdateri' +
      'ngar ...';
    StartButton := 'Starta';
    NextButton := 'N'#228'sta';
    ExitButton := 'Avsluta';
    RestartButton := 'Starta om';
    CancelButton := #197'ngra';
    FailedDownload := 'Misslyckades att h'#228'mta uppdateringar';
    GetUpdateButton := 'H'#228'mta uppdatering';
    NewVersionFound := 'Hittat ny version';
    NewVersion := 'Ny version';
    NoNewVersionAvail := 'Ny version saknas.';
    NewVersionAvail := 'Ny version finns';
    CurrentVersion := 'Aktuell version';
    NoFilesFound := 'Hittade inga uppdateringsbara filer';
    NoUpdateOnServer := 'hittade ingen uppdatering p'#229' servern ...';
    CannotConnect := 'Kunde inte f'#229' kontakt med servern eller';
    WhatsNew := 'Nyheter';
    License := 'Licensavtal';
    AcceptLicense := 'Jag accepterar';
    NotAcceptLicense := 'Jag accepterar inte';
    ComponentsAvail := 'Tillg'#228'ngliga applikationskomponenter';
    DownloadingFiles := 'H'#228'mtar filer';
    CurrentProgress := 'P'#229'g'#229'ende filf'#246'rlopp';
    TotalProgress := 'Totalt filf'#246'rlopp';
    UpdateComplete := 'Uppdateringen klar ...';
    RestartInfo := 'Tryck p'#229' "Starta om" f'#246'r att starta den uppdaterade applikatione' +
      'n';
    WhatsNewPopup := 'Visa i anteckningar';
    LicensePopup := 'Visa i Anteckningar';
end;

{ TWebUpdateWizardCzech }

constructor TWebUpdateWizardCzech.Create(AOwner: TComponent);
begin
  inherited;
    Welcome := 'Stisknìte spustit pro ovìøení existence nové verze aplikace ...';
    StartButton := 'Spustit';
    NextButton := 'Další';
    ExitButton := 'Ukonèit';
    RestartButton := 'Obnovit';
    CancelButton := 'Zrušit';
    FailedDownload := 'Nepodaøilo se získat novou verzi';
    GetUpdateButton := 'Získat novou verzi';
    NewVersionFound := 'Nalezena nová verze';
    NewVersion := 'Nová verze';
    NoNewVersionAvail := 'Nová verze není k dispozici.';
    NewVersionAvail := 'Nová verze je k dispozici.';
    CurrentVersion := 'Souèasná verze';
    NoFilesFound := 'Nenalezeny soubory nové verze';
    NoUpdateOnServer := 'žádná nová verze nenalezena ...';
    CannotConnect := 'Nebylo možno se pøipojit nebo';
    WhatsNew := 'Co je nového';
    License := 'Licenèní smlouva';
    AcceptLicense := 'Souhlasím';
    NotAcceptLicense := 'Nesouhlasím';
    ComponentsAvail := 'Dostupné souèásti aplikace';
    DownloadingFiles := 'Stahování souborù';
    CurrentProgress := 'Stav aktuálního souboru';
    TotalProgress := 'Stav všech souborù';
    UpdateComplete := 'Stažení nové verze ukonèeno...';
    RestartInfo := 'Stisknìte obnovit pro aplikování zmìn.,';
    WhatsNewPopup := 'Otevøít v Poznámkovém bloku';
    LicensePopup := 'Otevøít v Poznámkovém bloku';
end;

{ TWebUpdateWizardPolish }

constructor TWebUpdateWizardPolish.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Naciœnij START, aby sprawdziæ dostêpnoœæ uaktualnieñ...';
  StartButton := 'Start';
  NextButton := 'Dalej';
  ExitButton := 'Wyjœcie';
  RestartButton := 'Zakoñcz';
  CancelButton := 'Anuluj';
  GetUpdateButton := 'Pobierz';
  NewVersionFound := 'Znaleziono now¹ wersjê';
  NewVersion := 'Nowa wersja';
  NoNewVersionAvail := 'Nie znaleziono nowszej wersji';
  NewVersionAvail := 'Dostêpna jest nowsza wersja';
  CurrentVersion := 'Aktualna wersja';
  NoFilesFound := 'Nie odnaleziono plików na serwerze';
  NoUpdateOnServer := 'brak aktualizacji na serwerze!';
  CannotConnect := 'Brak po³¹czenia z serwerem lub';
  WhatsNew := 'Co nowego?';
  License := 'Licencja';
  AcceptLicense := 'Akceptujê licencjê' ;
  NotAcceptLicense := 'Nie akceptujê licencji';
  ComponentsAvail := 'Dostêpne komponenty';
  DownloadingFiles := 'Pobieranie plików';
  CurrentProgress := 'Postêp aktualnego pliku';
  TotalProgress := '£¹czny postêp';
  FailedDownload := 'Nie uda³o siê pobraæ uaktualnienia';
  UpdateComplete := 'Aktualizacja zakoñczona !';
  RestartInfo := 'Naciœnij ZAKOÑCZ, aby uruchomiæ now¹ wersjê aplikacji';
end;

{ TWebUpdateWizardCatalan }

constructor TWebUpdateWizardCatalan.Create(AOwner: TComponent);
begin
  inherited;
  Welcome := 'Premi Inicia para cercar actualitzacions disponibles de l''aplicació ...';
  StartButton := 'Inicia';
  NextButton := 'Següent';
  ExitButton := 'Surt';
  CancelButton := 'Cancel·la';
  RestartButton := 'Reinicia';
  GetUpdateButton := 'Obten actualització';
  NewVersionFound := 'S''ha trobat una nova versió';
  NewVersion := 'Nova versió';
  NoNewVersionAvail := 'No hi ha cap nova versió disponible.';
  NewVersionAvail := 'Hi ha una nova versió disponible.';
  CurrentVersion := 'Versió actual';
  NoFilesFound := 'No s''han trobat els fitxers per actualitzar';
  NoUpdateOnServer := 'no s''ha trobat cap nova actualització al servidor ...';
  CannotConnect := 'No es pot establir la connexió amb el servidor d''actualitzacions o ';
  WhatsNew := 'Què hi ha de nou';
  License := 'Acord de llicència';
  AcceptLicense := 'Accepto';
  NotAcceptLicense := 'No accepto';
  ComponentsAvail := 'Components de l''aplicació disponibles';
  DownloadingFiles := 'Descarregant fitxers';
  CurrentProgress := 'Progrés del fitxer actual';
  TotalProgress := 'Progrés total';
  UpdateComplete := 'Actualització completada ...';
  RestartInfo := 'Premi Reiniciar per executar l''aplicació actualitzada.';
  FailedDownload := 'No s''ha pogut descarregar l''actualització';
  LicensePopup := 'Veure amb Notepad';
  WhatsNewPopup := 'Veure amb Notepad';
end;


end.
