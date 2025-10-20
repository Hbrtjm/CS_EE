require Ecto.Query

defmodule Pollutiondb.Station do
  use Ecto.Schema
  schema "stations" do
    field :name, :string
    field :lon, :float
    field :lat, :float
    has_many :readings, Pollutiondb.Reading
  end

  def get_all do
    Pollutiondb.Repo.all(Station)
  end

  def get_id(id) do
    Pollutiondb.Repo.get(Station, id)
  end

  def delete(station) do
    Pollutiondb.Repo.delete(station)
  end

  def find_by_name(name) do
    Pollutiondb.Repo.all(Ecto.Query.where(Pollutiondb.Station, name: ^name) )
  end
  def find_by_location(lon,lat) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon == ^lon,
      where: s.lat == ^lat)
    |> Pollutiondb.Repo.all
  end

  def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon >= ^lon_min,
                      where: s.lon <= ^lon_max,
      where: s.lat >= ^lat_min,
                      where: s.lat <= ^lat_max)
    |> Pollutiondb.Repo.all
  end

  def update_name(station, newname) do
    Ecto.Changeset.cast(station, %{name: newname}, [:name])
    |> Ecto.Changeset.validate_required([:name])
    |> Pollutiondb.Repo.update
  end

end
