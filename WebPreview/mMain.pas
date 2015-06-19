// -----------------------------------------------------------------------------
// WebƒvƒŒƒrƒ…[
//
// Copyright (c) Kuro. All Rights Reserved.
// e-mail: info@haijin-boys.com
// www:    http://www.haijin-boys.com/
// -----------------------------------------------------------------------------

unit mMain;

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Menus, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.OleCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls, ExtCtrls, OleCtrls,
{$IFEND}
  SHDocVw, UWBDragDropContainer, UCustomDropTarget, mPerMonitorDpi;

type
  TMainForm = class(TScaledForm, IDropHandler)
    PopupMenu: TPopupMenu;
    BackMenuItem: TMenuItem;
    ForwardMenuItem: TMenuItem;
    N1: TMenuItem;
    RefreshMenuItem: TMenuItem;
    N2: TMenuItem;
    PropMenuItem: TMenuItem;
    WebBrowser: TWebBrowser;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PopupMenuPopup(Sender: TObject);
    procedure BackMenuItemClick(Sender: TObject);
    procedure ForwardMenuItemClick(Sender: TObject);
    procedure RefreshMenuItemClick(Sender: TObject);
    procedure PropMenuItemClick(Sender: TObject);
    procedure WebBrowserCommandStateChange(ASender: TObject; Command: Integer;
      Enable: WordBool);
    procedure WebBrowserNavigateComplete2(ASender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
  private
    { Private éŒ¾ }
    FEditor: THandle;
    FBarPos: NativeInt;
    FUpdateWebPreview: Boolean;
    FWBContainer: TWBDragDropContainer;
    FBackEnabled: Boolean;
    FForwardEnabled: Boolean;
    FFileName: string;
    FPoint: TPoint;
    procedure ReadIni;
    procedure WriteIni;
  protected
    { Protected éŒ¾ }
    procedure HandleText(const Text: string);
    procedure HandleFiles(const Files: TStrings);
  public
    { Public éŒ¾ }
    procedure WebPreviewAll;
    procedure SetScale(const Value: NativeInt);
    function SetProperties: Boolean;
    property BarPos: NativeInt read FBarPos write FBarPos;
    property UpdateWebPreview: Boolean read FUpdateWebPreview write FUpdateWebPreview;
    property Editor: THandle read FEditor write FEditor;
  end;

var
  MainForm: TMainForm;
  FFont: TFont;

implementation

uses
{$IF CompilerVersion > 22.9}
  System.Types, Winapi.ActiveX, System.IniFiles,
{$ELSE}
  Types, ActiveX, IniFiles,
{$IFEND}
  MSHTML, mCommon, mPlugin, mProp;

{$R *.dfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  if Win32MajorVersion < 6 then
    with Font do
    begin
      Name := 'Tahoma';
      Size := 8;
    end;
  FEditor := ParentWindow;
  FFont.Assign(Font);
  FUpdateWebPreview := False;
  FFileName := '';
  FPoint.X := 0;
  FPoint.Y := 0;
  ReadIni;
  with Font do
  begin
    ChangeScale(FFont.Size, Size);
    Name := FFont.Name;
    Size := FFont.Size;
  end;
  OleInitialize(nil);
  FWBContainer := TWBDragDropContainer.Create(WebBrowser);
  FWBContainer.DropTarget := TCustomDropTarget.Create(Self);
  FWBContainer.UseCustomCtxMenu := True;
  WebBrowser.Navigate('about:blank');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteIni;
  FWBContainer.DropTarget := nil;
  FreeAndNil(FWBContainer);
  OleUninitialize;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  //
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

procedure TMainForm.PopupMenuPopup(Sender: TObject);
begin
  BackMenuItem.Enabled := FBackEnabled;
  ForwardMenuItem.Enabled := FForwardEnabled;
end;

procedure TMainForm.BackMenuItemClick(Sender: TObject);
begin
  if FBackEnabled then
    WebBrowser.GoBack;
end;

procedure TMainForm.ForwardMenuItemClick(Sender: TObject);
begin
  if FForwardEnabled then
    WebBrowser.GoForward;
end;

procedure TMainForm.RefreshMenuItemClick(Sender: TObject);
begin
  WebPreviewAll;
end;

procedure TMainForm.PropMenuItemClick(Sender: TObject);
begin
  SetProperties;
end;

procedure TMainForm.WebBrowserCommandStateChange(ASender: TObject;
  Command: Integer; Enable: WordBool);
begin
  case Command of
    CSC_NAVIGATEFORWARD:
      FForwardEnabled := Enable;
    CSC_NAVIGATEBACK:
      FBackEnabled := Enable;
  end;
end;

procedure TMainForm.WebBrowserNavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  OLEVariant(WebBrowser.Document as IHTMLDocument2).ParentWindow.Scroll(FPoint.X, FPoint.Y);
end;

procedure TMainForm.ReadIni;
var
  S: string;
begin
  if not GetIniFileName(S) then
    Exit;
  with TMemIniFile.Create(S, TEncoding.UTF8) do
    try
      with FFont do
        if ValueExists('MainForm', 'FontName') then
        begin
          Name := ReadString('MainForm', 'FontName', Name);
          Size := ReadInteger('MainForm', 'FontSize', Size);
          Height := MulDiv(Height, 96, Screen.PixelsPerInch);
        end
        else if (Win32MajorVersion > 6) or ((Win32MajorVersion = 6) and (Win32MinorVersion >= 2)) then
        begin
          Assign(Screen.IconFont);
          Height := MulDiv(Height, 96, Screen.PixelsPerInch);
        end;
    finally
      Free;
    end;
end;

procedure TMainForm.WriteIni;
var
  S: string;
begin
  if FIniFailed or (not GetIniFileName(S)) then
    Exit;
  try
    with TMemIniFile.Create(S, TEncoding.UTF8) do
      try
        WriteInteger('WebPreview', 'CustomBarPos', FBarPos);
        UpdateFile;
      finally
        Free;
      end;
  except
    FIniFailed := True;
  end;
end;

procedure TMainForm.HandleText(const Text: string);
begin
  //
end;

procedure TMainForm.HandleFiles(const Files: TStrings);
var
  I: NativeInt;
  S: string;
begin
  for I := 0 to Files.Count - 1 do
  begin
    S := Files[I];
    Editor_LoadFile(FEditor, True, @S[1]);
  end;
end;

procedure TMainForm.WebPreviewAll;
var
  S: array [0 .. MAX_PATH] of Char;
begin
  Editor_Info(FEditor, MI_GET_FILE_NAME, LPARAM(@S));
  if S <> '' then
  begin
    if SameFileName(S, FFileName) then
    begin
      FPoint.X := OLEVariant(WebBrowser.Document as IHTMLDocument2).body.scrollLeft;
      FPoint.Y := OLEVariant(WebBrowser.Document as IHTMLDocument2).body.scrollTop;
    end
    else
    begin
      FPoint.X := 0;
      FPoint.Y := 0;
    end;
    WebBrowser.Silent := True;
    WebBrowser.Navigate(S, navNoReadFromCache or navNoWriteToCache);
    FFileName := S;
  end;
end;

procedure TMainForm.SetScale(const Value: NativeInt);
var
  P: NativeInt;
begin
  P := PixelsPerInch;
  PixelsPerInch := Value;
  with Font do
    Height := MulDiv(Height, Self.PixelsPerInch, P);
end;

function TMainForm.SetProperties: Boolean;
begin
  Result := False;
  if Prop(Self, FBarPos) then
  begin
    WriteIni;
    FUpdateWebPreview := True;
    Result := True;
  end;
end;

initialization

FFont := TFont.Create;

finalization

if Assigned(FFont) then
  FreeAndNil(FFont);

end.
