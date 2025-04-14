using Dash
using EcologicalNetworksDynamics
using Plots

# Function to generate a niche model food web and simulate dynamics
function simulate_food_web(richness, connectance, time_steps, FR)
    foodweb = Foodweb(niche_model(richness, connectance))
    params = default_model(foodweb, ClassicResponse(; h = FR))
    B0 = rand(richness)  # Random initial biomass
    sim = simulate(params, B0, time_steps)
    return sim
end

# Dash app layout
app = dash(
    external_stylesheets = ["https://codepen.io/chriddyp/pen/bWLwgP.css"]
)

app.layout = html_div([
    html_h1("Food Web Simulation Dashboard"),

    # Slider for species richness
    html_div([
        html_label("Species Richness:"),
        dcc_slider(
            id = "species-richness-slider",
            min = 2,
            max = 100,
            step = 1,
            value = 20,
            marks = Dict(i => string(i) for i in 0:10:100)
        )
    ], style = Dict("margin-bottom" => "20px")),

    # Slider for connectance
    html_div([
        html_label("Connectance:"),
        dcc_slider(
            id = "connectance-slider",
            min = 0.01,
            max = 0.5,
            step = 0.01,
            value = 0.1,
            marks = Dict(i => string(round(i, digits=2)) for i in 0:0.1:0.5)
        )
    ], style = Dict("margin-bottom" => "20px")),

    # Slider for functional response
    html_div([
        html_label("Functional response:"),
        dcc_slider(
            id = "functional-response-slider",
            min = 1,
            max = 2,
            step = 0.1,
            value = 1,
            marks = Dict(i => string(i) for i in 1:0.1:2)
        )
    ], style = Dict("margin-bottom" => "20px")),

    # Slider for PPMR
    html_div([
        html_label("Predator-Prey Mass Ratio:"),
        dcc_slider(
            id = "ppmr-slider",
            marks=Dict([i => ("$(logrange(0.01,1000, length = 6)[i])") for i = 1:6]),
            max=6,
            value=3,
            step=0.01,
        )
    ], style = Dict("margin-bottom" => "20px")),

    # Placeholder for biomass dynamics plot
    dcc_graph(id="biomass-plot")
])

# Callback for updating the simulation plot
callback!(
    app,
    Output("biomass-plot", "figure"),
    [
        Input("species-richness-slider", "value"),
        Input("connectance-slider", "value"),
        Input("functional-response-slider", "value")
    ]
) do richness, connectance, FR
    try
        # Simulate the food web dynamics
        time_steps = 300
        sim = simulate_food_web(richness, connectance, time_steps, FR)

        # Extract data for all species
        time = sim.t
        biomass = Array(sim.u)  # Ensure biomass is a standard array for indexing

        # Create a Dash-compatible figure with all species
        fig = Dict(
            "data" => [
                Dict(
                    "x" => time,
                    "y" => getindex.(biomass, i),  # Correct indexing for each species
                    "type" => "line",
                    "name" => "Species $i"
                ) for i in 1:richness  # Iterate over columns (species)
            ],
            "layout" => Dict(
                "title" => "Biomass Dynamics Over Time",
                "xaxis" => Dict("title" => "Time"),
                "yaxis" => Dict("title" => "Biomass (arbitrary units)")
            )
        )
        return fig
    catch e
        return Dict("data" => [], "layout" => Dict("title" => "Error: $(string(e))"))
    end
end

# Run the app
run_server(app, debug=true)