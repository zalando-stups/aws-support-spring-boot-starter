package org.zalando.automata.aws;

import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.context.config.ConfigFileApplicationListener;
import org.springframework.boot.env.EnvironmentPostProcessor;
import org.springframework.core.Ordered;
import org.springframework.core.env.CommandLinePropertySource;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertiesPropertySource;

import com.amazonaws.util.EC2MetadataUtils;

/**
 *
 */
public class AwsEnvironmentPostProcessor implements EnvironmentPostProcessor, Ordered {

	private final Logger log = LoggerFactory.getLogger(AwsEnvironmentPostProcessor.class);

	/**
	 *
	 */
	public static final String AWS_PREFIX = "aws";

	/**
	 *
	 */
	public static final String AWS_ENABLED_KEY = AWS_PREFIX + ".enabled";

	/**
	 *
	 */
	public static final String AWS_AZ_KEY = AWS_PREFIX + ".az";

	/**
	 *
	 */
	public static final String AWS_REGION_KEY = AWS_PREFIX + ".region";

	/**
	 *
	 */
	public static final String AWS_HOSTNAME_KEY = AWS_PREFIX + ".localhostname";

	/**
	 *
	 */
	public static final String AWS_LOCAL_IPV4_KEY = AWS_PREFIX + ".localipv4";

	/**
	 *
	 */
	public static final String AWS_INSTANCE_ID_KEY = AWS_PREFIX + ".instanceid";

	// Before ConfigFileApplicationListener so values there can use these ones
	private int order = ConfigFileApplicationListener.DEFAULT_ORDER - 1;

	/**
	 *
	 * @param order
   */
	public void setOrder(int order) {
		this.order = order;
	}

	@Override
	public int getOrder() {
		return order;
	}

	@Override
	public void postProcessEnvironment(ConfigurableEnvironment environment, SpringApplication application) {
		log.info("Detecting 'AWS'-Environment ...");
		Properties properties = new Properties();
		if (awsMetadataServiceIsAvailable()) {
			log.info("Collect 'AWS'-metadata ...");
			properties.put(AWS_ENABLED_KEY, Boolean.TRUE.toString());
			properties.put(AWS_AZ_KEY, getAwsAvailabilityZone());
			properties.put(AWS_REGION_KEY, getAwsRegion());
			properties.put(AWS_HOSTNAME_KEY, getAwsLocalHostname());
			properties.put(AWS_LOCAL_IPV4_KEY, getAwsLocalIpV4());
			properties.put(AWS_INSTANCE_ID_KEY, getInstanceId());
			log.info("'AWS'-metadata : {}", properties.toString());
		} else {
			log.info("Ignore 'AWS', no metadata available.");
			properties.put(AWS_ENABLED_KEY, Boolean.FALSE.toString());
		}

		//
		MutablePropertySources propertySources = environment.getPropertySources();
		if (propertySources.contains(CommandLinePropertySource.COMMAND_LINE_PROPERTY_SOURCE_NAME)) {
			propertySources.addAfter(CommandLinePropertySource.COMMAND_LINE_PROPERTY_SOURCE_NAME,
					new PropertiesPropertySource("aws", properties));
		} else {
			propertySources.addFirst(new PropertiesPropertySource("aws", properties));
		}
	}

	private String getAwsLocalIpV4() {
		try {
			return EC2MetadataUtils.getNetworkInterfaces().get(0).getLocalIPv4s().get(0);
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 *
	 * @return
	 */
	protected String getAwsAvailabilityZone() {
		return EC2MetadataUtils.getAvailabilityZone();
	}

	/**
	 *
	 * @return
	 */
	protected String getAwsRegion() {
		return EC2MetadataUtils.getEC2InstanceRegion();
	}

	/**
	 *
	 * @return
	 */
	protected String getAwsLocalHostname() {
		return EC2MetadataUtils.getLocalHostName();
	}

	/**
	 *
	 * @return
	 */
	protected String getInstanceId() {
		return EC2MetadataUtils.getInstanceId();
	}

	/**
	 *
	 * @return
	 */
	protected boolean awsMetadataServiceIsAvailable() {
		return EC2MetadataUtils.getInstanceId() != null;
	}
}
