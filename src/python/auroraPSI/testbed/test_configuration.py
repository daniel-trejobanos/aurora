from auroraPSI.config.config import AuroraConfiguration


def test_configuration_observations():
    expected_dictionary = {
        "time": "acsm_utc_time",
        "amus": "amus",
        "data": "Org_Specs",
        "error": "OrgSpecs_err"
    }
    local_configuration = AuroraConfiguration()
    assert local_configuration.observations == expected_dictionary