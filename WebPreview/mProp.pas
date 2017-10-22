unit mProp;

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
{$IFEND}
  mMain, mPerMonitorDpi;

type
  TCenterForm = class(TScaledForm)
  private
    { Private éŒ¾ }
  protected
    { Protected éŒ¾ }
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoShow; override;
  public
    { Public éŒ¾ }
    constructor Create(AOwner: TComponent); override;
  end;

  TPropForm = class(TCenterForm)
    BarPosLabel: TLabel;
    BarPosComboBox: TComboBox;
    AutoDisplayCheckBox: TCheckBox;
    ModeListView: TListView;
    Bevel: TBevel;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AutoDisplayCheckBoxClick(Sender: TObject);
  private
    { Private éŒ¾ }
  public
    { Public éŒ¾ }
  end;

function Prop(AOwner: TComponent; var APos: Integer;
  var AAutoDisplay: Boolean; AModeList: TStringList; AModes: TStrings): Boolean;

var
  PropForm: TPropForm;

implementation

{$R *.dfm}


uses
{$IF CompilerVersion > 22.9}
  System.Math, Winapi.MultiMon,
{$ELSE}
  Math, MultiMon,
{$IFEND}
  mCommon;

function Prop(AOwner: TComponent; var APos: Integer;
  var AAutoDisplay: Boolean; AModeList: TStringList; AModes: TStrings): Boolean;
var
  S: string;
  I: Integer;
begin
  with TPropForm.Create(AOwner) do
    try
      BarPosComboBox.ItemIndex := APos;
      AutoDisplayCheckBox.Checked := AAutoDisplay;
      for S in AModes do
        with ModeListView.Items.Add do
        begin
          Caption := S;
          Checked := AModeList.IndexOf(S) > -1;
        end;
      Result := ShowModal = mrOk;
      if Result then
      begin
        APos := BarPosComboBox.ItemIndex;
        AAutoDisplay := AutoDisplayCheckBox.Checked;
        AModeList.Clear;
        with ModeListView do
          for I := 0 to Items.Count - 1 do
            with Items[I] do
              if Checked then
                AModeList.Add(Caption);
      end;
    finally
      Release;
    end;
end;

{ TCenterForm }

constructor TCenterForm.Create(AOwner: TComponent);
var
  AppMon, WinMon: HMONITOR;
  I, J: Integer;
  LLeft, LTop: Integer;
begin
  inherited;
  AppMon := Screen.MonitorFromWindow(GetParent(Handle), mdNearest).Handle;
  WinMon := Monitor.Handle;
  for I := 0 to Screen.MonitorCount - 1 do
    if Screen.Monitors[I].Handle = AppMon then
      if AppMon <> WinMon then
        for J := 0 to Screen.MonitorCount - 1 do
          if Screen.Monitors[J].Handle = WinMon then
          begin
            LLeft := Screen.Monitors[I].Left + Left - Screen.Monitors[J].Left;
            if LLeft + Width > Screen.Monitors[I].Left + Screen.Monitors[I].Width then
              LLeft := Screen.Monitors[I].Left + Screen.Monitors[I].Width - Width;
            LTop := Screen.Monitors[I].Top + Top - Screen.Monitors[J].Top;
            if LTop + Height > Screen.Monitors[I].Top + Screen.Monitors[I].Height then
              LTop := Screen.Monitors[I].Top + Screen.Monitors[I].Height - Height;
            SetBounds(LLeft, LTop, Width, Height);
          end;
end;

procedure TCenterForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := TWinControl(Owner).Handle;
end;

procedure TCenterForm.DoShow;
var
  H: THandle;
  R1, R2: TRect;
begin
  H := GetParent(Handle);
  if (H = 0) or IsIconic(H) then
    H := GetDesktopWindow;
  if GetWindowRect(H, R1) and GetWindowRect(Handle, R2) then
    SetWindowPos(Handle, 0,
      R1.Left + (((R1.Right - R1.Left) - (R2.Right - R2.Left)) div 2),
      R1.Top + (((R1.Bottom - R1.Top) - (R2.Bottom - R2.Top)) div 2),
      0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
  inherited;
end;

{ TPropForm }

procedure TPropForm.FormCreate(Sender: TObject);
begin
  //
end;

procedure TPropForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TPropForm.FormShow(Sender: TObject);
begin
  AutoDisplayCheckBoxClick(nil);
end;

procedure TPropForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

procedure TPropForm.AutoDisplayCheckBoxClick(Sender: TObject);
begin
  ModeListView.Enabled := AutoDisplayCheckBox.Checked;
end;

end.
